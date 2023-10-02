use bevy::{
    // input::mouse::{MouseMotion, MouseWheel},
    prelude::*,
    render::camera::OrthographicProjection,
    window::PrimaryWindow,
    // sprite::{MaterialMesh2dBundle, Mesh2dHandle},
};

// use crate::canvas::*;
// use crate::markers::SpawnMarkersEvent;
// use crate::util::*;

#[derive(Resource)]
pub(crate) struct Cursor {
    pub position: Vec2,
    pub pos_relative_to_click: Vec2,
    pub last_click_position: Vec2,
}

impl Default for Cursor {
    fn default() -> Self {
        Cursor {
            position: Vec2::ZERO,
            pos_relative_to_click: Vec2::ZERO,
            last_click_position: Vec2::ZERO,
        }
    }
}

pub(crate) fn record_mouse_events_system(
    mut cursor_moved_events: EventReader<CursorMoved>,
    mouse_button_input: Res<Input<MouseButton>>,
    mut cursor_res: ResMut<Cursor>,
    primary_query: Query<&Window, With<PrimaryWindow>>,
    cam_transform_query: Query<&Transform, With<OrthographicProjection>>,
    cam_ortho_query: Query<&OrthographicProjection>,
) {
    let primary_window = primary_query.get_single().unwrap();
    for event in cursor_moved_events.iter() {
        let cursor_in_pixels = event.position; // lower left is origin
        let window_size = Vec2::new(primary_window.width(), primary_window.height());

        let screen_position = cursor_in_pixels - window_size / 2.0;

        let cam_transform = cam_transform_query.iter().next().unwrap();

        // this variable currently has no effect
        let mut scale = 1.0;

        for ortho in cam_ortho_query.iter() {
            scale = ortho.scale;
        }

        let cursor_vec4: Vec4 = cam_transform.compute_matrix()
            * screen_position.extend(0.0).extend(1.0 / (scale))
            * scale;

        let cursor_pos = Vec2::new(cursor_vec4.x, cursor_vec4.y);
        cursor_res.position = cursor_pos;
        cursor_res.pos_relative_to_click = cursor_res.position - cursor_res.last_click_position;
    }

    if mouse_button_input.just_pressed(MouseButton::Left) {
        cursor_res.last_click_position = cursor_res.position;
        cursor_res.pos_relative_to_click = Vec2::ZERO;
    }
}
