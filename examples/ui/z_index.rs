//! Demonstrates how to use the z-index component on UI nodes to control their relative depth
//!
//! It uses colored boxes with different z-index values to demonstrate how it can affect the order of
//! depth of nodes compared to their siblings, but also compared to the entire UI.

use bevy::{
    color::palettes::basic::{BLUE, GRAY, LIME, PURPLE, RED, YELLOW},
    prelude::*,
};

fn main() {
    App::new()
        .insert_resource(ClearColor(Color::BLACK))
        .add_plugins(DefaultPlugins)
        .add_systems(Startup, setup)
        .run();
}

fn setup(mut commands: Commands) {
    commands.spawn(Camera2d);

    // spawn the container with default z-index.
    // the default z-index value is `ZIndex(0)`.
    // because this is a root UI node, using local or global values will do the same thing.
    commands
        .spawn(Node {
            width: Val::Percent(100.),
            height: Val::Percent(100.),
            align_items: AlignItems::Center,
            justify_content: JustifyContent::Center,
            ..default()
        })
        .with_children(|parent| {
            parent
                .spawn((
                    Node {
                        width: Val::Px(180.0),
                        height: Val::Px(100.0),
                        ..default()
                    },
                    BackgroundColor(GRAY.into()),
                ))
                .with_children(|parent| {
                    // spawn a node with default z-index.
                    parent.spawn((
                        Node {
                            position_type: PositionType::Absolute,
                            left: Val::Px(10.0),
                            bottom: Val::Px(40.0),
                            width: Val::Px(100.0),
                            height: Val::Px(50.0),
                            ..default()
                        },
                        BackgroundColor(RED.into()),
                    ));

                    // spawn a node with a positive local z-index of 2.
                    // it will show above other nodes in the gray container.
                    parent.spawn((
                        Node {
                            position_type: PositionType::Absolute,
                            left: Val::Px(45.0),
                            bottom: Val::Px(30.0),
                            width: Val::Px(100.),
                            height: Val::Px(50.),
                            ..default()
                        },
                        ZIndex(2),
                        BackgroundColor(BLUE.into()),
                    ));

                    // spawn a node with a negative local z-index.
                    // it will show under other nodes in the gray container.
                    parent.spawn((
                        Node {
                            position_type: PositionType::Absolute,
                            left: Val::Px(70.0),
                            bottom: Val::Px(20.0),
                            width: Val::Px(100.),
                            height: Val::Px(75.),
                            ..default()
                        },
                        ZIndex(-1),
                        BackgroundColor(LIME.into()),
                    ));

                    // spawn a node with a positive global z-index of 1.
                    // it will show above all other nodes, because it's the highest global z-index in this example.
                    // by default, boxes all share the global z-index of 0 that the gray container is added to.
                    parent.spawn((
                        Node {
                            position_type: PositionType::Absolute,
                            left: Val::Px(15.0),
                            bottom: Val::Px(10.0),
                            width: Val::Px(100.),
                            height: Val::Px(60.),
                            ..default()
                        },
                        BackgroundColor(PURPLE.into()),
                        GlobalZIndex(1),
                    ));

                    // spawn a node with a negative global z-index of -1.
                    // this will show under all other nodes including its parent, because it's the lowest global z-index
                    // in this example.
                    parent.spawn((
                        Node {
                            position_type: PositionType::Absolute,
                            left: Val::Px(-15.0),
                            bottom: Val::Px(-15.0),
                            width: Val::Px(100.),
                            height: Val::Px(125.),
                            ..default()
                        },
                        BackgroundColor(YELLOW.into()),
                        GlobalZIndex(-1),
                    ));
                });
        });
}
