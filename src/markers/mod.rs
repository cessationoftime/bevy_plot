use bevy::{
    core_pipeline::core_2d::Transparent2d,
    ecs::system::lifetimeless::{Read, SQuery, SRes},
    ecs::system::SystemParamItem,
    log::*,
    prelude::*,
    reflect::TypeUuid,
    render::{
        extract_component::{ComponentUniforms, DynamicUniformIndex, UniformComponentPlugin},
        extract_component::{ExtractComponent, ExtractComponentPlugin},
        mesh::{GpuBufferInfo, MeshVertexBufferLayout},
        render_asset::RenderAssets,
        render_phase::{
            AddRenderCommand, DrawFunctions, RenderCommand, RenderCommandResult, RenderPhase,
            SetItemPipeline, TrackedRenderPass,
        },
        render_resource::{ShaderType, *},
        renderer::RenderDevice,
        view::VisibleEntities,
        view::{ComputedVisibility, Msaa, Visibility},
        Render, RenderApp, RenderSet,
    },
    sprite::{
        Mesh2dHandle, Mesh2dPipeline, Mesh2dPipelineKey, Mesh2dUniform, SetMesh2dBindGroup,
        SetMesh2dViewBindGroup,
    },
    utils::FloatOrd,
};

use bytemuck::{Pod, Zeroable};

use crate::plot::*;
use crate::util::*;

// TODOs:
// 1) Modify the transform instead of spawning brand new entities
// this way, the uniform will stay the same

pub(crate) fn markers_setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut change_canvas_material_event: EventReader<RespawnAllEvent>,
    mut plots: ResMut<Assets<Plot>>,
    query: Query<(Entity, &Handle<Plot>), With<MarkerUniform>>,
) {
    info!("entering markers_setup function");

    for event in change_canvas_material_event.iter() {
        for (entity, plot_handle) in query.iter() {
            if event.plot_handle == *plot_handle {
                commands.entity(entity).despawn();
            }
        }

        let plot = plots.get_mut(&event.plot_handle).unwrap();

        plot_points(
            &mut commands,
            &mut meshes,
            // ys,
            plot,
            &event.plot_handle,
        )
    }
    info!("markers_setup function completed")
}

fn plot_points(
    commands: &mut Commands,
    meshes: &mut ResMut<Assets<Mesh>>,
    plot: &Plot,
    plot_handle: &Handle<Plot>,
) {
    let data = plot.data.clone();
    // let color = data.marker_plot.color;
    for marker_plot in data.marker_groups.iter() {
        let ys = marker_plot.data.clone();
        // let color = marker_plot.color;
        // let ys_world = plot.plot_to_local(&ys);
        let ys_world = ys.iter().map(|y| plot.to_local(*y)).collect::<Vec<Vec2>>();

        let quad_size = 30.0;

        let marker_uniform = MarkerUniform {
            marker_size: marker_plot.size,
            hole_size: 1.0,
            zoom: 1.0,
            marker_type: marker_plot.marker_style.to_int32(),
            marker_point_color: col_to_vec4(marker_plot.marker_point_color),
            color: col_to_vec4(marker_plot.color),
            quad_size,
            inner_canvas_size_in_pixels: plot.canvas_size / (1.0 + plot.outer_border),
            // outer_border: plot.outer_border,
            canvas_position: plot.canvas_position,
            contour: if marker_plot.draw_contour { 1.0 } else { 0.0 },
        };

        info!("Creating MarkerUniform: {:?}", marker_uniform);

        let my_entity = commands
            .spawn((
                Mesh2dHandle(meshes.add(Mesh::from(shape::Quad {
                    size: Vec2::splat(quad_size),
                    flip: false,
                }))),
                GlobalTransform::default(),
                Transform::from_translation(Vec3::new(0.0, 0.0, 1.12)),
                Visibility::default(),
                ComputedVisibility::default(),
                MarkerInstanceMatData(
                    ys_world
                        .iter()
                        .map(|v| MarkerInstanceData {
                            //
                            // TODO: take inner border into account
                            //
                            position: Vec3::new(v.x, v.y, 0.0) + plot.canvas_position.extend(0.000),
                            scale: 1.0,
                            color: Color::rgba(0.8, 0.6, 0.1, 1.0).as_rgba_f32(),
                        })
                        .collect(),
                ),
                // NoFrustumCulling,
            ))
            .insert(plot_handle.clone())
            .insert(marker_uniform)
            .id();

        info!(
            "Attached MarkerUniform to entity: {:?} with Handle<Plot>: {:?}",
            my_entity, plot_handle
        );
    }
}

#[derive(Component)]
pub(crate) struct MarkerInstanceMatData(Vec<MarkerInstanceData>);
impl ExtractComponent for MarkerInstanceMatData {
    type Query = &'static MarkerInstanceMatData;
    type Filter = ();
    type Out = MarkerInstanceMatData;

    fn extract_component(item: bevy::ecs::query::QueryItem<Self::Query>) -> Option<Self::Out> {
        Some(MarkerInstanceMatData(item.0.clone()))
    }
}

#[derive(Component, Default)]
pub(crate) struct MarkerMesh2d;

/// Uniform sent to markers.wgsl
#[derive(Component, Clone, ShaderType, Debug)]
pub(crate) struct MarkerUniform {
    pub marker_size: f32,
    /// When the ```marker_point_color``` field is different from the ```color``` field,
    /// there is a small visible circle within the marker. ```hole_size``` controls the size of the circle.
    pub hole_size: f32,
    pub zoom: f32,
    pub marker_type: i32,
    /// Size of the instanced square quad for one marker.
    pub quad_size: f32,

    /// Shows a black contour around the marker if the value is > 0.5.
    pub contour: f32,
    pub inner_canvas_size_in_pixels: Vec2,
    pub canvas_position: Vec2,
    pub color: Vec4,

    /// Color of the small circle within the marker.
    pub marker_point_color: Vec4,
}

// TODO: we have instance data, but we don't use it at the moment.
// One use case would be to have marker size as an additional dimension.

#[derive(Clone, Copy, Pod, Zeroable)]
#[repr(C)]
struct MarkerInstanceData {
    position: Vec3,
    scale: f32,
    color: [f32; 4],
}

/// Custom pipeline for 2d meshes with vertex colors
#[derive(Resource)]
pub(crate) struct MarkerMesh2dPipeline {
    /// this pipeline wraps the standard [`Mesh2dPipeline`]
    mesh2d_pipeline: Mesh2dPipeline,
    pub custom_uniform_layout: BindGroupLayout,
    // pub shader: Handle<Shader>,
    // material_layout: BindGroupLayout,
}

impl FromWorld for MarkerMesh2dPipeline {
    fn from_world(world: &mut World) -> Self {
        let mesh2d_pipeline = Mesh2dPipeline::from_world(world).clone();

        let render_device = world.get_resource::<RenderDevice>().unwrap();

        let custom_uniform_layout =
            render_device.create_bind_group_layout(&BindGroupLayoutDescriptor {
                entries: &[BindGroupLayoutEntry {
                    binding: 0,
                    visibility: ShaderStages::VERTEX | ShaderStages::FRAGMENT,
                    ty: BindingType::Buffer {
                        ty: BufferBindingType::Uniform,
                        has_dynamic_offset: true,
                        min_binding_size: BufferSize::new(MarkerUniform::min_size().get()),
                    },
                    count: None,
                }],
                label: Some("markers_uniform_layout"),
            });

        // let world = world.cell();
        // let asset_server = world.get_resource::<AssetServer>().unwrap();

        // let shader = asset_server.load("../assets/shaders/markers.wgsl");

        // let _result = asset_server.watch_for_changes();

        Self {
            mesh2d_pipeline,
            custom_uniform_layout,
            // shader,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct MarkerPipelineKey {
    mesh: Mesh2dPipelineKey,
    shader_handle: Handle<Shader>,
}

impl SpecializedMeshPipeline for MarkerMesh2dPipeline {
    type Key = MarkerPipelineKey;

    fn specialize(
        &self,
        key: Self::Key,
        layout: &MeshVertexBufferLayout,
    ) -> Result<RenderPipelineDescriptor, SpecializedMeshPipelineError> {
        info!("Starting pipeline specialization with key: {:?}", key);

        let mut descriptor = match self.mesh2d_pipeline.specialize(key.mesh, layout) {
            Ok(descriptor) => {
                info!("Base pipeline descriptor created successfully.");
                descriptor
            }
            Err(e) => {
                error!("Error creating base pipeline descriptor: {:?}", e);
                return Err(e);
            }
        };

        descriptor.vertex.shader = key.shader_handle.clone();
        descriptor.vertex.buffers.push(VertexBufferLayout {
            array_stride: std::mem::size_of::<MarkerInstanceData>() as u64,
            step_mode: VertexStepMode::Instance,
            attributes: vec![
                VertexAttribute {
                    format: VertexFormat::Float32x4,
                    offset: 0,
                    shader_location: 3, // shader locations 0-2 are taken up by Position, Normal and UV attributes
                },
                VertexAttribute {
                    format: VertexFormat::Float32x4,
                    offset: VertexFormat::Float32x4.size(),
                    shader_location: 4,
                },
                // VertexAttribute {
                //     format: VertexFormat::Float32x4,
                //     offset: VertexFormat::Float32x4.size(),
                //     shader_location: 5,
                // },
            ],
        });
        descriptor.fragment.as_mut().unwrap().shader = key.shader_handle.clone();
        descriptor.layout = vec![
            self.mesh2d_pipeline.view_layout.clone(),
            self.custom_uniform_layout.clone(),
            self.mesh2d_pipeline.mesh_layout.clone(),
        ];

        info!("Pipeline specialization completed successfully.");

        Ok(descriptor)
    }
}

// This specifies how to render a colored 2d mesh
type DrawMarkerMesh2d = (
    // Set the pipeline
    SetItemPipeline,
    // Set the view uniform as bind group 0
    SetMesh2dViewBindGroup<0>,
    // Set the marker uniform as bind group 1
    SetMarkerUniformBindGroup<1>,
    // Set the mesh uniform as bind group 2
    SetMesh2dBindGroup<2>,
    // Draw the mesh
    DrawMarkerMeshInstanced,
);

pub(crate) struct MarkerMesh2dPlugin;

#[derive(Resource)]
pub(crate) struct MarkerShaderHandle(pub Handle<Shader>);

pub const MARKER_SHADER_HANDLE: HandleUntyped =
    HandleUntyped::weak_from_u64(Shader::TYPE_UUID, 9826352034109932589);

impl Plugin for MarkerMesh2dPlugin {
    fn build(&self, app: &mut App) {
        // load_internal_asset!(app, MARKER_SHADER_HANDLE, "markers.wgsl", Shader::from_wgsl);

        info!("Starting MarkerMesh2dPlugin setup");
        let mut shaders = app.world.get_resource_mut::<Assets<Shader>>().unwrap();

        let handle_untyped = MARKER_SHADER_HANDLE.clone();

        shaders.set_untracked(
            handle_untyped.clone(),
            Shader::from_wgsl(include_str!("markers.wgsl"), "markers.wgsl"),
        );

        let shader_typed_handle = shaders.get_handle(handle_untyped);

        //example for UniformComponent plugin is here:
        //https://github.com/bevyengine/bevy/blob/v0.11.3/examples/shader/post_processing.rs
        app.add_plugins((
            ExtractComponentPlugin::<MarkerInstanceMatData>::default(),
            UniformComponentPlugin::<MarkerUniform>::default(),
        ))
        .sub_app_mut(RenderApp)
        // Use the RenderApp sub-app to register our custom draw function and pipeline, and add our render systems
        .add_render_command::<Transparent2d, DrawMarkerMesh2d>()
        .insert_resource(MarkerShaderHandle(shader_typed_handle))
        .add_systems(Render, prepare_instance_buffers.in_set(RenderSet::Prepare))
        .add_systems(
            Render,
            extract_colored_mesh2d.in_set(RenderSet::ExtractCommands),
        )
        .add_systems(
            Render,
            prepare_marker_uniform_bind_group.in_set(RenderSet::Queue),
        ) // in 0.12 we will need to change the above from RenderSet::Queue to RenderSet::PrepareBindGroups as per the changes made here: https://github.com/bevyengine/bevy/pull/9236        )
        .add_systems(Render, queue_colored_mesh2d.in_set(RenderSet::Queue));
        info!("MarkerMesh2dPlugin setup complete");
    }

    fn finish(&self, app: &mut App) {
        info!("Finalizing MarkerMesh2dPlugin");
        if let Ok(render_app) = app.get_sub_app_mut(RenderApp) {
            render_app
                .init_resource::<MarkerMesh2dPipeline>()
                .init_resource::<SpecializedMeshPipelines<MarkerMesh2dPipeline>>();
        }
        info!("MarkerMesh2dPlugin finalization complete");
    }
}

/// Extract MarkerUniform
fn extract_colored_mesh2d(
    mut commands: Commands,
    mut previous_len: Local<usize>,
    query: Query<(Entity, &MarkerUniform, &ComputedVisibility), With<MarkerInstanceMatData>>,
) {
    let mut values = Vec::with_capacity(*previous_len);
    for (entity, custom_uni, computed_visibility) in query.iter() {
        if !computed_visibility.is_visible() {
            continue;
        }
        values.push((entity, (custom_uni.clone(), MarkerMesh2d)));

        info!("Extracting MarkerUniform from entity: {:?}", entity);
        info!("MarkerUniform data: {:?}", custom_uni);
    }

    *previous_len = values.len();
    commands.insert_or_spawn_batch(values);
}

fn prepare_instance_buffers(
    mut commands: Commands,
    query: Query<(Entity, &MarkerInstanceMatData)>,
    render_device: Res<RenderDevice>,
) {
    for (entity, instance_data) in query.iter() {
        let buffer = render_device.create_buffer_with_data(&BufferInitDescriptor {
            label: Some("marker instance data buffer"),
            contents: bytemuck::cast_slice(instance_data.0.as_slice()),
            usage: BufferUsages::VERTEX | BufferUsages::COPY_DST,
        });
        commands.entity(entity).insert(MarkerInstanceBuffer {
            buffer,
            length: instance_data.0.len(),
        });
    }
}

#[derive(Resource)]
struct MarkerUniformBindGroup {
    pub value: BindGroup,
}

fn prepare_marker_uniform_bind_group(
    mut commands: Commands,
    mesh2d_pipeline: Res<MarkerMesh2dPipeline>,
    render_device: Res<RenderDevice>,
    mesh2d_uniforms: Res<ComponentUniforms<MarkerUniform>>,
) {
    info!("Entering queue_marker_uniform_bind_group, about to create MarkerUniformBindGroup");
    if let Some(binding) = mesh2d_uniforms.uniforms().binding() {
        let bind_group = render_device.create_bind_group(&BindGroupDescriptor {
            entries: &[BindGroupEntry {
                binding: 0,
                resource: binding,
            }],
            label: Some("MarkersUniform_bind_group"),
            layout: &mesh2d_pipeline.custom_uniform_layout,
        });

        info!("MarkerUniformBindGroup created successfully.");

        commands.insert_resource(MarkerUniformBindGroup { value: bind_group });
    } else {
        error!("MarkerUniformBindGroup resource creation failed: Binding not available.");
    }
}

#[allow(clippy::too_many_arguments)]
fn queue_colored_mesh2d(
    transparent_draw_functions: Res<DrawFunctions<Transparent2d>>,
    colored_mesh2d_pipeline: Res<MarkerMesh2dPipeline>,
    mut pipelines: ResMut<SpecializedMeshPipelines<MarkerMesh2dPipeline>>,
    pipeline_cache: Res<PipelineCache>,
    msaa: Res<Msaa>,
    render_meshes: Res<RenderAssets<Mesh>>,
    shader_handle: Res<MarkerShaderHandle>,
    colored_mesh2d: Query<(&Mesh2dHandle, &Mesh2dUniform), With<MarkerInstanceMatData>>,
    mut views: Query<(&VisibleEntities, &mut RenderPhase<Transparent2d>)>,
) {
    if colored_mesh2d.is_empty() {
        return;
    }

    // Iterate each view (a camera is a view)
    for (visible_entities, mut transparent_phase) in views.iter_mut() {
        let draw_colored_mesh2d = transparent_draw_functions
            .read()
            .get_id::<DrawMarkerMesh2d>()
            .unwrap();

        // let mesh_key = Mesh2dPipelineKey::from_msaa_samples(msaa.samples);

        let mesh_key = MarkerPipelineKey {
            mesh: Mesh2dPipelineKey::from_msaa_samples(msaa.samples()),
            shader_handle: shader_handle.0.clone(),
        };

        // Queue all entities visible to that view
        for visible_entity in &visible_entities.entities {
            if let Ok((mesh2d_handle, mesh2d_uniform)) = colored_mesh2d.get(*visible_entity) {
                let mut mesh2d_key = mesh_key.clone();
                if let Some(mesh) = render_meshes.get(&mesh2d_handle.0) {
                    mesh2d_key.mesh |=
                        Mesh2dPipelineKey::from_primitive_topology(mesh.primitive_topology);

                    if let Ok(pipeline_id) = pipelines.specialize(
                        &pipeline_cache,
                        &colored_mesh2d_pipeline,
                        mesh2d_key,
                        &mesh.layout.clone(),
                    ) {
                        let mesh_z = mesh2d_uniform.transform.w_axis.z;
                        transparent_phase.add(Transparent2d {
                            entity: *visible_entity,
                            draw_function: draw_colored_mesh2d,
                            pipeline: pipeline_id,
                            sort_key: FloatOrd(mesh_z),
                            batch_range: None,
                        });
                    }
                } else {
                    error!("error in queue_colored_mesh_2d, unable to get render_meshes");
                }
            }
        }
    }
}

struct SetMarkerUniformBindGroup<const I: usize>;
impl<const I: usize> RenderCommand<Transparent2d> for SetMarkerUniformBindGroup<I> {
    type Param = (
        SRes<MarkerUniformBindGroup>,
        SQuery<Read<DynamicUniformIndex<MarkerUniform>>>,
    );

    type ViewWorldQuery = ();
    type ItemWorldQuery = ();

    #[inline]
    fn render<'w>(
        _item: &Transparent2d,
        _view: bevy::ecs::query::ROQueryItem<'w, Self::ViewWorldQuery>,
        _entity: bevy::ecs::query::ROQueryItem<'w, Self::ItemWorldQuery>,
        (mesh2d_bind_group, mesh2d_query): SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let mesh2d_index = mesh2d_query.get_single().unwrap();

        pass.set_bind_group(
            I,
            &mesh2d_bind_group.into_inner().value,
            &[mesh2d_index.index()],
        );
        RenderCommandResult::Success
    }
}

#[derive(Component)]
struct MarkerInstanceBuffer {
    buffer: Buffer,
    length: usize,
}

struct DrawMarkerMeshInstanced;
impl RenderCommand<Transparent2d> for DrawMarkerMeshInstanced {
    type Param = (
        SRes<RenderAssets<Mesh>>,
        SQuery<Read<Mesh2dHandle>>,
        SQuery<Read<MarkerInstanceBuffer>>,
    );

    type ViewWorldQuery = ();
    type ItemWorldQuery = Entity;

    #[inline]
    fn render<'w>(
        _item: &Transparent2d,
        _view: bevy::ecs::query::ROQueryItem<'w, Self::ViewWorldQuery>,
        entity: bevy::ecs::query::ROQueryItem<'w, Self::ItemWorldQuery>,
        (meshes, mesh_query, instance_buffer_query): SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let mesh_handle = &mesh_query.get_single().unwrap().0; //.get().unwrap().0;
        let instance_buffer = instance_buffer_query.get_inner(entity).unwrap();

        info!("Mesh Handle: {:?}", mesh_handle);
        info!("Instance Buffer Length: {}", instance_buffer.length);

        let gpu_mesh = match meshes.into_inner().get(mesh_handle) {
            Some(gpu_mesh) => gpu_mesh,
            None => return RenderCommandResult::Failure,
        };

        pass.set_vertex_buffer(0, gpu_mesh.vertex_buffer.slice(..));
        pass.set_vertex_buffer(1, instance_buffer.buffer.slice(..));

        match &gpu_mesh.buffer_info {
            GpuBufferInfo::Indexed {
                buffer,
                index_format,
                count,
            } => {
                pass.set_index_buffer(buffer.slice(..), 0, *index_format);
                info!("About to execute draw_indexed command");
                pass.draw_indexed(0..*count, 0, 0..instance_buffer.length as u32);
            }
            GpuBufferInfo::NonIndexed => {
                info!("About to execute draw command");
                pass.draw(0..gpu_mesh.vertex_count, 0..instance_buffer.length as u32);
            }
        }

        info!("Draw command executed successfully");

        RenderCommandResult::Success
    }
}

// impl RenderCommand<Transparent2d> for DrawMarkerMeshInstanced {
//     type Param = (
//         SRes<RenderAssets<Mesh>>,
//         SQuery<Read<Mesh2dHandle>>,
//         SQuery<Read<MarkerInstanceBuffer>>,
//     );

//     #[inline]
//     fn render<'w>(
//         _view: Entity,
//         item: Entity,
//         (meshes, mesh_query, instance_buffer_query): SystemParamItem<'w, '_, Self::Param>,
//         pass: &mut TrackedRenderPass<'w>,
//     ) -> RenderCommandResult {
//         let mesh_handle = &mesh_query.get(item).unwrap().0;
//         let instance_buffer = instance_buffer_query.get_inner(item).unwrap();

//         let gpu_mesh = match meshes.into_inner().get(mesh_handle) {
//             Some(gpu_mesh) => gpu_mesh,
//             None => return RenderCommandResult::Failure,
//         };

//         pass.set_vertex_buffer(0, gpu_mesh.vertex_buffer.slice(..));
//         pass.set_vertex_buffer(1, instance_buffer.buffer.slice(..));

//         match &gpu_mesh.buffer_info {
//             GpuBufferInfo::Indexed {
//                 buffer,
//                 index_format,
//                 count,
//             } => {
//                 pass.set_index_buffer(buffer.slice(..), 0, *index_format);
//                 pass.draw_indexed(0..*count, 0, 0..instance_buffer.length as u32);
//             }
//             GpuBufferInfo::NonIndexed => {
//                 pass.draw(0..gpu_mesh.vertex_count, 0..instance_buffer.length as u32);
//             }
//         }
//         RenderCommandResult::Success
//     }
// }
