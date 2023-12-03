use bevy::reflect::TypePath;
use bevy::{
    prelude::*, reflect::TypeUuid, render::render_resource::ShaderType, render::Render,
    render::RenderSet, sprite::Material2dPlugin,
};

use super::colors::make_color_palette;
use super::plot_format::*;

use crate::bezier::*;
use crate::canvas::*;

use crate::inputs::*;
use crate::markers::*;
use crate::segments::*;
use crate::util::*;

#[derive(Hash, Debug, Clone, PartialEq, Eq, SystemSet)]
enum PlotSytems {
    Model,
    ShaderUpdate,
    Other,
    Segment,
    Marker,
}

/// Main plugin for bevy_plot
pub struct PlotPlugin;

// z planes from bottom to top:
//
// canvas: 0.0001
// text and labels: 1.0001
// bezier 1.10
// segments: 1.11
// markers: 1.12
// target text: 1.2

// TODO:
// 1) Area under the curve
// 2) Automatically color curve, segments and markers with palette
// 3) Global variable for z planes
// 4) optimize
// 5) add 3d mesh materials

impl Plugin for PlotPlugin {
    fn build(&self, app: &mut App) {
        app.configure_sets(
            Update,
            (PlotSytems::Model,).before(PlotSytems::ShaderUpdate),
        );

        app.configure_sets(
            Update,
            (PlotSytems::ShaderUpdate,).before(PlotSytems::Other),
        );

        app.configure_sets(Update, (PlotSytems::Other,).before(PlotSytems::Segment));

        app.configure_sets(Update, (PlotSytems::Segment,).before(PlotSytems::Marker));

        app
            // canvas
            .add_plugins(Material2dPlugin::<CanvasMaterial>::default())
            .add_plugins(MarkerMesh2dPlugin)
            .add_plugins(BezierMesh2dPlugin)
            .add_plugins(SegmentMesh2dPlugin)
            .add_plugins(CanvasMesh2dPlugin)
            .add_event::<SpawnGraphEvent>()
            .add_event::<ReleaseAllEvent>()
            .add_event::<UpdatePlotLabelsEvent>()
            .add_event::<RespawnAllEvent>()
            .add_event::<WaitForUpdatePlotLabelsEvent>()
            .add_event::<UpdateTargetLabelEvent>()
            .add_event::<UpdateBezierShaderEvent>()
            .add_event::<SpawnBezierCurveEvent>()
            .add_asset::<Plot>()
            .insert_resource(make_color_palette())
            .insert_resource(Cursor::default())
            .insert_resource(TickLabelFont { maybe_font: None })
            //comment out this extraction system when done debugging
            .add_systems(
                ExtractSchedule,
                log_marker_uniform_extraction.in_set(RenderSet::ExtractCommands),
            )
            .add_systems(
                Update,
                (adjust_graph_axes, change_plot).in_set(PlotSytems::Model),
            )
            .add_systems(
                Update,
                (
                    update_bezier_uniform,
                    spawn_bezier_function,
                    wait_for_graph_spawn,
                )
                    .in_set(PlotSytems::ShaderUpdate),
            )
            .add_systems(
                Update,
                (
                    release_all,
                    spawn_graph,
                    adjust_graph_size,
                    record_mouse_events_system,
                    update_mouse_target,
                    update_plot_labels,
                    update_target,
                    do_spawn_plot,
                    animate_bezier,
                )
                    .in_set(PlotSytems::Other),
            )
            .add_systems(Update, (segments_setup).in_set(PlotSytems::Segment));
        // .add_systems(Update, (markers_setup).in_set(PlotSytems::Marker));
    }
}

fn log_marker_uniform_extraction(query: Query<(Entity, &MarkerUniform), With<MarkerUniform>>) {
    for (entity, marker_uniform) in query.iter() {
        info!(
            "MarkerUniform present for entity {:?}: {:?}",
            entity, marker_uniform
        );
    }
}

fn do_spawn_plot(
    mut commands: Commands,
    mut plots: ResMut<Assets<Plot>>,
    query: Query<(Entity, &Handle<Plot>)>,
    mut spawn_plot_event: EventWriter<SpawnGraphEvent>,
) {
    for (entity, plot_handle) in query.iter() {
        let plot = plots.get_mut(plot_handle).unwrap();
        if plot.do_spawn_plot {
            let canvas = plot.make_canvas();

            spawn_plot_event.send(SpawnGraphEvent {
                canvas,
                plot_handle: plot_handle.clone(),
            });

            plot.do_spawn_plot = false;

            // To access the plot handle, earlier we spawned a dummy entity with the plot handle.
            // This entity's purpose has been served and it is time to despawn it already.
            commands.entity(entity).despawn();
        }
    }
}

/// Handle to the type of font to use for tick labels. If None is given (default),
/// the tick labels are not rendered.
#[derive(Resource)]
pub struct TickLabelFont {
    pub maybe_font: Option<Handle<Font>>,
}

/// Upon modifying any of the plot fields, use this event to update the the view (shaders).
/// For updating a scatter plot (markers) or a regular plot (segments), send
/// the RespawnAllEvent event. Bevy Plot will then despawn all the entities and respawn
/// them with the updated information.
#[derive(Event)]
pub struct RespawnAllEvent {
    pub plot_handle: Handle<Plot>,
}

/// See the animate.rs example, where [`UpdateBezierShaderEvent`] is used to tell bevy_plot that
/// the view for an explicit function needs to be updated.
///
#[derive(Event)]
pub struct UpdateBezierShaderEvent {
    pub plot_handle: Handle<Plot>,
    pub entity: Entity,
    pub group_number: usize,
}

#[derive(Event)]
pub(crate) struct WaitForUpdatePlotLabelsEvent {
    pub plot_handle: Handle<Plot>,
    pub quad_entity: Entity,
}

/// Component that serves as identification for the nth curve group of the `bezier_groups` field
/// of [`PlotData`].
#[derive(Component)]
pub struct BezierCurveNumber(pub usize);

/// Lower and upper bounds for the canvas. The x axis (or horizontal axis) ranges from `lo.x` to `up.x` and
/// the `y` axis ranges from `lo.y` to `up.y`.
#[repr(C)]
#[derive(Debug, Clone, Copy, ShaderType, bytemuck::Pod, bytemuck::Zeroable)]
pub(crate) struct PlotCanvasBounds {
    pub up: Vec2,
    pub lo: Vec2,
}

#[derive(Debug, Clone)]
/// Struct containing the data to be plotted and metaparameters of any explicit function plot.
/// It can be found in  the `data.bezier_groups` sub-field of a [`Plot`]. The reason for its name is
/// that bevy_plot interpolates between samples of the function using quadratic bezier curves.
pub struct BezierData {
    /// Function to be displayed
    pub function: fn(f32, f32) -> f32,
    /// Thickness of the segments
    pub size: f32,
    /// Not implemented yet
    pub line_style: LineStyle,

    /// Color of the curve
    pub color: Color,

    /// If true, the function is displayed with visual mechanical joints
    pub mech: bool,

    /// The number of samples that bevy_plot uses to draw the
    /// function with quadratic interpolation between each sample
    pub num_points: usize,

    /// If true, bevy_plot recomputes the `function` field every frame
    pub show_animation: bool,
}

impl Default for BezierData {
    fn default() -> Self {
        BezierData {
            function: |x: f32, _t: f32| x,
            color: Color::rgb(0.2, 0.3, 0.8),
            size: 1.0,
            line_style: LineStyle::Solid,
            mech: false,
            num_points: 256,
            show_animation: false,
        }
    }
}

/// Struct containing the data to be plotted and metaparameters of a marker (or scatter) plot.
/// It can be found in the `data.marker_groups` sub-field of a [`Plot`].
#[derive(Debug, Clone)]
pub struct MarkerData {
    /// The data to be displayed in the scatter plot
    pub data: Vec<Vec2>,

    /// The main color of the markers
    pub color: Color,

    /// The color of tiny circle centered exactly at the data point
    pub marker_point_color: Color,

    /// Determines the shape of the markers
    pub marker_style: MarkerStyle,

    /// Size of the markers, clamped between 0.2 and 2.0
    pub size: f32,

    /// If true, the markers are displayed with a black border
    pub draw_contour: bool,
}

impl Default for MarkerData {
    fn default() -> Self {
        MarkerData {
            data: vec![],
            color: Color::rgb(0.5, 0.5, 0.1),
            marker_point_color: Color::rgb(0.2, 0.3, 0.8),
            marker_style: MarkerStyle::Circle,
            size: 1.0,
            draw_contour: false,
        }
    }
}

/// Struct containing the data to be plotted and metaparameters of a segment (or regular) plot.
/// It can be found in  the `data.segment_groups` sub-field of a [`Plot`].
#[derive(Debug, Clone)]
pub struct SegmentData {
    /// The data to be displayed in the regular plot
    pub data: Vec<Vec2>,
    /// Color of the segments
    pub color: Color,
    /// Thickness of the segments
    pub size: f32,
    /// If the `line_style` is set to `LineStyle::None`, the segments are not drawn
    pub line_style: LineStyle,
    pub draw_contour: bool,
    pub mech: bool,
}

impl Default for SegmentData {
    fn default() -> Self {
        SegmentData {
            data: vec![],
            color: Color::hex("8eb274").unwrap(),
            size: 1.0,
            line_style: LineStyle::Solid,
            draw_contour: false,
            mech: false,
        }
    }
}

/// The data for each type of plot has to be accessed though this struct first. Each element of a `Vec`
/// corresponds to a particular curve on the graph.
#[derive(Debug, Clone, Default)]
pub struct PlotData {
    pub marker_groups: Vec<MarkerData>,
    pub segment_groups: Vec<SegmentData>,
    pub bezier_groups: Vec<BezierData>,
}

/// Type of markers for a given marker plot.
#[derive(Debug, Clone, PartialEq)]
pub enum MarkerStyle {
    None,
    Circle,
    Square,
    Triangle,
    Heart,
    Cross,
    Rhombus,
    Star,
    Moon,
    X,
}

impl MarkerStyle {
    pub fn to_int32(&self) -> i32 {
        match self {
            MarkerStyle::None => -1,
            MarkerStyle::Square => 0,
            MarkerStyle::Heart => 1,
            MarkerStyle::Triangle => 3,
            MarkerStyle::Rhombus => 2,
            MarkerStyle::Star => 4,
            MarkerStyle::Moon => 5,
            MarkerStyle::Cross => 6,
            MarkerStyle::X => 7,
            MarkerStyle::Circle => 8,
        }
    }
}

/// The ```None``` variant can be used to avoid spawning the
/// segments of a regular plot when calling plotopt(), leaving only the markers.
#[derive(Debug, Clone, PartialEq)]
pub enum LineStyle {
    None,
    Solid,
    // // unimplemented
    // Dashed,
    // Dotted,
    // DashDot,
    // DashDotDot,
}

impl LineStyle {
    pub fn to_int32(&self) -> i32 {
        match self {
            LineStyle::None => -1,
            LineStyle::Solid => 0,
            // LineStyle::Dashed => 1,
            // LineStyle::Dotted => 2,
            // LineStyle::DashDot => 3,
            // LineStyle::DashDotDot => 4,
        }
    }
}

/// Options for customizing the appearance of the plot.
#[derive(Debug, Clone, PartialEq)]
// Options as the second argument the of plotop method
pub enum Opt {
    /// Main color. Shared between [`Plot::plotopt_func`]` and [`Plot::plotopt`]
    Color(Color),

    /// Thickness of a curve or segment. Shared between [`Plot::plotopt_func`]` and [`Plot::plotopt`]
    Size(f32),

    /// Either [`LineStyle::None`] or [`LineStyle::Solid`]. The former can be used to
    /// avoid spawning either the segments or the bezier curves, depending on the type of plot.
    LineStyle(LineStyle),

    /// If true, the shader will draw joints between the segments of a regular plot or the
    /// parts of a func curve.
    Mech(bool),

    /// Determines the number of separate parts in a func plot.
    /// Works with [`Plot::plotopt`] only.
    NumPoints(usize),

    /// If true, bevy_plot computes the `function` field of [`BezierData`] at every frame.
    /// Needs to be used in conjunction with a function that explicitly depends on time.
    Animate(bool),

    /// Main color of the markers.
    MarkerColor(Color),

    /// Size of the markers.
    MarkerSize(f32),

    /// Determines the shape of the markers.
    MarkerStyle(MarkerStyle),

    /// Color of the tiny circle centered exactly at the data point. To turn this features off,
    /// simply chose the same color as the `MarkerColor`.
    MarkerInnerPointColor(Color),

    /// If true, the markers are displayed with a black border.
    Contour(bool),
}

/// Contains all relevant information to both the look of the canvas and the data to be plotted.
#[derive(Debug, Clone, Component, TypeUuid, TypePath)]
#[uuid = "a6354c45-cc21-48f7-99cc-8c1924d2427b"]

pub struct Plot {
    /// mouse position in the reference frame of the graph, corresponding to its axes
    pub plot_coord_mouse_pos: Vec2,

    /// Position of the canvas in `World` coordinates
    pub canvas_position: Vec2,

    /// Distance between consecutive grid lines
    pub tick_period: Vec2,

    /// Size of the margins with respect to the canvas_size. The default is set to `Vec2::new(0.03 * size.y / size.x, 0.03)`
    pub outer_border: Vec2,

    /// Size of the graph in pixels
    pub canvas_size: Vec2,

    /// Color of even tiles
    pub background_color1: Color,

    /// Color of odd tiles
    pub background_color2: Color,

    /// The grid is shown by default
    pub show_grid: bool,

    /// Position of the origin of the graph in `World` coordinates
    pub zero_world: Vec2,

    /// unused
    pub time: f32,

    /// The current zoom value: adjustable with the `MouseWheel`
    pub zoom: f32,

    /// Hides the black contour around the canvas
    pub hide_contour: bool,

    /// Hides numeric labels by the side of the grid lines
    pub hide_tick_labels: bool,

    /// Hides half the numeric tick labels for a less crowded feel
    pub hide_half_ticks: bool,

    /// Color for the numerical labels shown by the side of the grid lines
    pub tick_label_color: Color,

    /// Adjusts the number of significant digits for the tick labels
    pub significant_digits: usize,

    /// A target can be spawned together with a pair of coordinates by pressing `MouseButton::Middle`
    pub show_target: bool,

    /// The color for the coordinate pair by the side of the target
    pub target_label_color: Color,

    /// Color of the target
    pub target_color: Color,

    /// Number of significant digits for the target coordinates
    pub target_significant_digits: usize,

    /// Axes are shown by default
    pub show_axes: bool,

    /// The number of samples taken on the explicit function provided to [`Plot::plot_func`]` or [`Plot::plotopt_func`] functions
    pub bezier_num_points: usize,

    /// Contains the data and metaparameters needed for drawing each kind of plot
    pub data: PlotData,

    pub(crate) target_position: Vec2,
    pub(crate) target_toggle: bool,
    pub(crate) bounds: PlotCanvasBounds,
    pub(crate) bezier_dummy: f32,
    pub(crate) do_spawn_plot: bool,
}

impl Default for Plot {
    fn default() -> Plot {
        let size = Vec2::new(800.0, 500.0);

        let mut plot = Plot {
            plot_coord_mouse_pos: Vec2::ZERO,

            tick_period: Vec2::new(0.2, 0.2),

            bounds: PlotCanvasBounds {
                up: Vec2::new(1.2, 1.2),
                lo: Vec2::new(-0.2, -0.2),
            },

            time: 0.0,
            zoom: 1.0,

            show_grid: true,
            background_color1: Color::rgba(0.048, 0.00468, 0.0744, 1.0),
            background_color2: Color::rgba(0.0244, 0.0023, 0.0372, 1.0),

            canvas_size: size,
            outer_border: Vec2::new(0.03 * size.y / size.x, 0.03),
            zero_world: Vec2::new(0.0, 0.0),

            hide_contour: false,
            hide_tick_labels: false,
            hide_half_ticks: true,
            significant_digits: 2,
            show_axes: true,
            show_target: false,
            target_toggle: false,
            tick_label_color: Color::BLACK,
            target_label_color: Color::GRAY,
            target_color: Color::GRAY,
            target_position: Vec2::new(0.0, 0.0),
            target_significant_digits: 2,

            canvas_position: Vec2::ZERO,

            data: PlotData::default(),

            bezier_num_points: 100,
            bezier_dummy: 0.0,

            do_spawn_plot: true,
        };

        plot.compute_zeros();
        plot
    }
}

impl Plot {
    /// Customizable plotting function. Takes any type that implements [`Plotable`], namely
    ///  `Vec<Vec2>`, `Vec<(f64, f64)>`, `Vec<f32>`, ...
    pub fn plotopt<T: Plotable>(&mut self, v: T, options: Vec<Opt>) {
        //
        let data_in_plot_format: PlotFormat = v.as_plot_format();

        if !options.contains(&Opt::LineStyle(LineStyle::None)) {
            let mut data = SegmentData {
                data: data_in_plot_format.data.clone(),
                ..Default::default()
            };

            for option in options.iter() {
                match option {
                    Opt::Color(col) => {
                        data.color = *col;
                    }

                    Opt::Size(si) => {
                        data.size = *si;
                    }
                    Opt::LineStyle(style) => {
                        data.line_style = style.clone();
                    }

                    Opt::Mech(mech) => {
                        data.mech = *mech;
                    }

                    _ => {}
                }
            }

            self.data.segment_groups.push(data);
        }

        // Decide whether to draw markers using the options.
        // If any of MarkerStyle or MarkerSize is specified, draw markers
        let draw_markers = options
            .iter()
            .map(|opt| matches!(opt, Opt::MarkerStyle(_) | Opt::MarkerSize(_)))
            .any(|x| x);

        if draw_markers {
            let mut data = MarkerData {
                data: data_in_plot_format.data.clone(),
                ..Default::default()
            };

            for option in options.iter() {
                match option {
                    Opt::MarkerColor(col) => {
                        data.color = *col;
                    }

                    Opt::MarkerSize(mut si) => {
                        si = si.clamp(0.2, 2.0);
                        data.size = si;
                    }
                    Opt::MarkerStyle(style) => {
                        data.marker_style = style.clone();
                    }
                    Opt::MarkerInnerPointColor(col) => {
                        data.marker_point_color = *col;
                    }
                    Opt::Contour(cont) => {
                        data.draw_contour = *cont;
                    }
                    _ => {}
                }
            }

            self.data.marker_groups.push(data);
        }
    }

    /// Quickly plot data points using segments to connect consecutive points. Takes any type
    /// that implements [`Plotable`], namely `Vec<Vec2>`, `Vec<(f64, f64)>`, `Vec<f32>`, ...
    pub fn plot(&mut self, v: impl Plotable) {
        //
        let pf: PlotFormat = v.as_plot_format();

        let data = &pf.data;

        let lo_x = data
            .iter()
            .min_by(|q, r| q.x.partial_cmp(&r.x).unwrap())
            .unwrap()
            .x;

        let lo_y = data
            .iter()
            .min_by(|q, r| q.y.partial_cmp(&r.y).unwrap())
            .unwrap()
            .y;

        let up_x = data
            .iter()
            .max_by(|q, r| q.x.partial_cmp(&r.x).unwrap())
            .unwrap()
            .x;

        let up_y = data
            .iter()
            .max_by(|q, r| q.y.partial_cmp(&r.y).unwrap())
            .unwrap()
            .y;

        let dx = (up_x - lo_x).abs() * 0.1;
        let dy = (up_y - lo_y).abs() * 0.1;

        self.set_bounds(
            Vec2::new(lo_x - dx, lo_y - dy),
            Vec2::new(up_x + dx, up_y + dy),
        );

        let new_data = SegmentData {
            data: pf.data,
            ..Default::default()
        };

        self.data.segment_groups.push(new_data);
    }

    /// Quickly plot data points using markers (scatter plot).
    pub fn plotm<T: Plotable>(&mut self, v: T) {
        //
        let pf: PlotFormat = v.as_plot_format();

        let data = pf.data;

        let lo_x = data
            .iter()
            .min_by(|q, r| q.x.partial_cmp(&r.x).unwrap())
            .unwrap()
            .x;

        let lo_y = data
            .iter()
            .min_by(|q, r| q.y.partial_cmp(&r.y).unwrap())
            .unwrap()
            .y;

        let up_x = data
            .iter()
            .max_by(|q, r| q.x.partial_cmp(&r.x).unwrap())
            .unwrap()
            .x;

        let up_y = data
            .iter()
            .max_by(|q, r| q.y.partial_cmp(&r.y).unwrap())
            .unwrap()
            .y;

        let dx = (up_x - lo_x).abs() * 0.1;
        let dy = (up_y - lo_y).abs() * 0.1;

        self.set_bounds(
            Vec2::new(lo_x - dx, lo_y - dy),
            Vec2::new(up_x + dx, up_y + dy),
        );

        let new_data = MarkerData {
            data,
            ..Default::default()
        };

        self.data.marker_groups.push(new_data);
    }

    /// Quickly plot a function by providing said function. Defaults to a range on the both axes from `-0.2` to `1.2`.
    pub fn plot_func(&mut self, f: fn(f32, f32) -> f32) {
        //

        let new_data = BezierData {
            function: f,
            ..Default::default()
        };

        self.data.bezier_groups.push(new_data);
    }

    /// Plot a function by providing said function and options.
    pub fn plotopt_func(&mut self, f: fn(f32, f32) -> f32, options: Vec<Opt>) {
        //
        let mut data = BezierData {
            function: f,
            ..Default::default()
        };

        for option in options.iter() {
            match option {
                Opt::Color(col) => {
                    data.color = *col;
                }

                Opt::Size(si) => {
                    data.size = *si;
                }
                Opt::LineStyle(style) => {
                    data.line_style = style.clone();
                }

                Opt::Mech(mech) => {
                    data.mech = *mech;
                }

                Opt::Animate(animate) => {
                    data.show_animation = *animate;
                }

                Opt::MarkerStyle(_) => {
                    eprintln!("MarkerStyle is not a valid option for segments");
                }

                Opt::MarkerInnerPointColor(_) => {
                    eprintln!("MarkerInnerPointColor is not a valid option for segments");
                }

                Opt::Contour(_) => {
                    println!("Contour is not a valid option for segments");
                }

                Opt::NumPoints(_) => {
                    eprintln!("NumPoints is not a valid option for segments");
                }

                Opt::MarkerColor(_) => {
                    eprintln!("MarkerColor is not a valid option for segments");
                }

                Opt::MarkerSize(_) => {
                    eprintln!("MarkerSize is not a valid option for segments");
                } // _ => {},
            }
        }
        self.data.bezier_groups.push(data);
    }

    fn make_canvas(&self) -> Canvas {
        Canvas {
            position: self.canvas_position,
            previous_position: self.canvas_position,
            original_size: self.canvas_size,
            scale: Vec2::splat(1.0),
            previous_scale: Vec2::splat(1.0),
            hover_radius: 20.0,
        }
    }

    pub(crate) fn delta_axes(&self) -> Vec2 {
        self.bounds.up - self.bounds.lo
    }

    pub(crate) fn zoom_axes(&mut self, direction: f32) {
        let percent_factor = 10.0;

        let multiplier = 1.0 + direction * percent_factor / 100.0;

        self.bounds.up =
            self.plot_coord_mouse_pos + (self.bounds.up - self.plot_coord_mouse_pos) * multiplier;
        self.bounds.lo =
            self.plot_coord_mouse_pos - (self.plot_coord_mouse_pos - self.bounds.lo) * multiplier;

        self.zoom *= multiplier;
    }

    pub(crate) fn move_axes(&mut self, mouse_delta: Vec2) {
        let mut axes = self.delta_axes();
        axes.x *= -1.0;
        let size = self.canvas_size / (1. + self.outer_border);

        self.bounds.up += mouse_delta * axes / size;
        self.bounds.lo += mouse_delta * axes / size;
    }

    // TODO: make a smarter tick period adjuster
    pub(crate) fn clamp_tick_period(&mut self) {
        let max_num_ticks = 15.0;
        let min_num_ticks = 0.000001;

        self.tick_period.x = self.tick_period.x.clamp(
            self.delta_axes().x / max_num_ticks,
            self.delta_axes().x / min_num_ticks,
        );

        self.tick_period.y = self.tick_period.y.clamp(
            self.delta_axes().y / max_num_ticks,
            self.delta_axes().x / min_num_ticks,
        );
    }

    /// Override the default plot bounds: x axis goes from bounds.lo.x to bounds.up.x.
    /// Beware! The tick period is automatically adjusted. Changing the tick period before setting the bounds will not have the intended effect.
    /// The bounds must be set before the ticks.
    ///
    /// # Panics
    ///
    /// Panics if `lo.x >= up.x` or `lo.y >= up.y`.
    pub fn set_bounds(&mut self, lo: Vec2, up: Vec2) {
        if lo.x >= up.x {
            panic!("when using plot.set_bounds(), lo.x must be strictly less than up.x");
        } else if lo.y >= up.y {
            panic!("when using plot.set_bounds(), lo.y must be strictly less than up.y");
        };

        self.bounds = PlotCanvasBounds { lo, up };

        let delta = up - lo;
        let exact_tick = delta / 10.0;

        // find order of magnitude of dx
        let order_x = exact_tick.x.log10().floor();
        let mag_x = 10_f32.powf(order_x);

        let p1x = mag_x * 1.0;
        let p2x = mag_x * 2.0;
        let p5x = mag_x * 5.0;

        let psx = [p1x, p2x, p5x];

        let vx = [
            (p1x - exact_tick.x).abs(),
            (p2x - exact_tick.x).abs(),
            (p5x - exact_tick.x).abs(),
        ];

        use std::cmp::Ordering;
        let min_x_index = vx
            .iter()
            .enumerate()
            .min_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(Ordering::Equal))
            .map(|(index, _)| index);

        let tick_x = psx[min_x_index.unwrap()];

        let order_y = exact_tick.y.log10().floor();
        let mag_y = 10_f32.powf(order_y);

        let p1y = mag_y * 1.0;
        let p2y = mag_y * 2.0;
        let p5y = mag_y * 5.0;

        let psy = [p1y, p2y, p5y];

        let vy = [
            (p1y - exact_tick.y).abs(),
            (p2y - exact_tick.y).abs(),
            (p5y - exact_tick.y).abs(),
        ];

        let min_y_index = vy
            .iter()
            .enumerate()
            .min_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(Ordering::Equal))
            .map(|(index, _)| index);

        let tick_y = psy[min_y_index.unwrap()];

        self.tick_period = Vec2::new(tick_x, tick_y);

        self.compute_zeros();
    }

    pub(crate) fn compute_zeros(&mut self) {
        let lo_world = -self.canvas_size / 2.0 / (1.0 + self.outer_border);

        let v = Vec2::new(
            self.bounds.lo.x * self.canvas_size.x
                / (1.0 + self.outer_border.x)
                / (self.bounds.up.x - self.bounds.lo.x),
            self.bounds.lo.y * self.canvas_size.y
                / (1.0 + self.outer_border.y)
                / (self.bounds.up.y - self.bounds.lo.y),
        );

        self.zero_world = lo_world - v;
    }

    pub(crate) fn compute_bounds_world(&self) -> PlotCanvasBounds {
        let lo = self.to_local(self.bounds.lo);
        let up = self.to_local(self.bounds.up);

        PlotCanvasBounds { up, lo }
    }

    /// Convert a point in plot coordinates to a point in world coordinates modulo the canvas position
    pub fn to_local(&self, v: Vec2) -> Vec2 {
        self.zero_world
            + v * self.canvas_size / (self.bounds.up - self.bounds.lo) / (1.0 + self.outer_border.x)
    }

    /// Convert a point in world coordinates to a point in the graph coordinates.
    pub fn world_to_plot(&self, y: Vec2) -> Vec2 {
        (y - self.zero_world - self.canvas_position) * (self.bounds.up - self.bounds.lo)
            / self.canvas_size
            * (1.0 + self.outer_border)
    }
}
