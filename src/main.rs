use bevy::{
    gltf::GltfPlugin,
    input::{
        keyboard::{KeyCode, KeyboardInput},
        ElementState,
    },
    prelude::*,
    ui::UiPlugin,
};
use bevy_kira_audio::{
    Audio, AudioPlugin, AudioSource, BeatEvent, TimelineSettings, TimelineState,
};
use bevy_obj::ObjPlugin;
use bevy_tweening::{
    component_animator_system, Animator, EaseFunction, Lens, Tween, TweeningPlugin, TweeningType,
};
use std::{
    any::Any,
    collections::HashSet,
    iter::Cycle,
    ops::{Add, AddAssign, Neg, RangeInclusive},
    slice::Iter,
    time::Duration,
};

#[derive(Component)]
struct IsPlayer;

#[derive(Component, Copy, Clone, PartialEq, Eq, Hash, Debug, Default)]
struct GridPosition {
    x: u8,
    y: u8,
}

impl GridPosition {
    fn checked_add(self, other: GridMovement) -> Option<Self> {
        let (new_x, new_y) = (
            self.x as i16 + other.x as i16,
            self.y as i16 + other.y as i16,
        );
        let (new_x, new_y) = (
            if new_x < 0 || new_x > u8::MAX as i16 {
                None
            } else {
                Some(new_x)
            },
            if new_y < 0 || new_y > u8::MAX as i16 {
                None
            } else {
                Some(new_y)
            },
        );
        Some(Self {
            x: new_x? as _,
            y: new_y? as _,
        })
    }
}

impl Add<GridMovement> for GridPosition {
    type Output = Self;

    fn add(self, other: GridMovement) -> Self {
        let (new_x, new_y) = (
            self.x as i16 + other.x as i16,
            self.y as i16 + other.y as i16,
        );
        let (new_x, new_y) = (new_x.clamp(0, u8::MAX as _), new_y.clamp(0, u8::MAX as _));
        Self {
            x: new_x as _,
            y: new_y as _,
        }
    }
}

impl AddAssign<GridMovement> for GridPosition {
    fn add_assign(&mut self, other: GridMovement) {
        *self = *self + other;
    }
}

#[derive(Component, Copy, Clone, PartialEq, Eq, Hash, Debug, Default)]
struct GridMovement {
    x: i8,
    y: i8,
}

impl Neg for GridMovement {
    type Output = Self;

    fn neg(self) -> Self {
        Self {
            x: -self.x,
            y: -self.y,
        }
    }
}

trait MakeAttack: Send + Sync + 'static {
    fn attack(
        &self,
        state: &mut dyn Any,
        owner: Entity,
        grid: &Grid,
        pos: &GridPosition,
    ) -> AttackComponent;
}

trait Attack: Send + Sync + 'static {
    fn hit(&self, pos: &GridPosition) -> Option<AttackData>;
    fn update(&mut self, beat: &BeatEvent) -> bool;
}

trait Mover: Send + Sync + 'static {
    fn choose_move(
        &self,
        state: &mut dyn Any,
        target: Option<&GridPosition>,
        grid: &Grid,
        pos: &GridPosition,
    ) -> Option<GridMovement>;
}

#[derive(Copy, Clone, Default)]
struct PatternStep {
    mover: Option<&'static dyn Mover>,
    attack: Option<&'static dyn MakeAttack>,
}

impl PatternStep {
    const fn nothing() -> Self {
        Self {
            mover: None,
            attack: None,
        }
    }

    const fn mover(mover: &'static dyn Mover) -> Self {
        Self {
            mover: Some(mover),
            attack: None,
        }
    }
    const fn attack(attack: &'static dyn MakeAttack) -> Self {
        Self {
            mover: None,
            attack: Some(attack),
        }
    }
}

struct PatternPlayer {
    sound: Handle<AudioSource>,
    state: Box<dyn Any + Send + Sync + 'static>,
    pattern: Cycle<Iter<'static, PatternStep>>,
    interval: BeatEvent,
}

#[derive(Component)]
struct Enemy {
    attack_pattern: PatternPlayer,
}

#[derive(Component)]
struct Target(Entity);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Component)]
enum Collision {
    Solid,
    NonSolid,
}

#[derive(Component, PartialEq, Eq, Hash, Debug)]
enum Team {
    Player,
    Enemy,
}

#[derive(Component)]
struct AttackComponent {
    owner: Entity,
    already_hit: HashSet<Entity>,
    attack: Box<dyn Attack>,
}

#[derive(Component)]
struct Alive {
    alive: bool,
}

#[derive(Clone, PartialEq, Eq, Hash, Component)]
struct AttackData {
    damage: u16,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug, Component)]
struct Health {
    amount: u16,
}

#[derive(Component)]
struct Grid {
    shape: Vec<Vec<Collision>>,
    size: f32,
}

impl Grid {
    fn can_move(&self, pos: GridPosition, dir: GridMovement) -> bool {
        pos.checked_add(dir)
            .map(|new_pos| self.at(new_pos) == Collision::NonSolid)
            .unwrap_or(false)
    }

    fn at(&self, pos: GridPosition) -> Collision {
        self.shape
            .get(pos.y as usize)
            .and_then(|row| row.get(pos.x as usize))
            .copied()
            .unwrap_or(Collision::Solid)
    }
}

const DEFAULT_BPM: u8 = 147;

#[derive(Component)]
struct GridElement {
    lightness: f32,
    color: Color,
}
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum AppState {
    Menu,
    Playing,
    Dead,
}

#[derive(Component)]
struct SceneRoot;

const DEFAULT_GRID_COLOR: Color = Color::rgb(0.3, 0.3, 0.3);
const DANGER_GRID_COLOR: Color = Color::rgb(0.87, 0.19, 0.49);

fn main() {
    App::new()
        .insert_resource(Msaa { samples: 4 })
        .insert_resource(ClearColor(Color::BLACK))
        .add_state(AppState::Menu)
        .add_plugins_with(DefaultPlugins, |group| {
            group.add_before::<bevy::asset::AssetPlugin, _>(
                bevy_embedded_assets::EmbeddedAssetPlugin,
            )
        })
        .add_plugin(AudioPlugin)
        .add_plugin(GltfPlugin)
        .add_plugin(ObjPlugin)
        .add_plugin(TweeningPlugin)
        .add_plugin(UiPlugin)
        .add_system(remove_dead)
        .add_system(component_animator_system::<GridElement>)
        .add_system(component_animator_system::<Alive>)
        .add_system(set_grid_attack_colors)
        .add_system(update_grid_elements)
        .add_system_set(
            SystemSet::on_enter(AppState::Playing).with_system(start_playing.label("spawn_world")),
        )
        .add_system_set(SystemSet::on_enter(AppState::Menu).with_system(spawn_menu))
        .add_system_set(SystemSet::on_update(AppState::Menu).with_system(handle_button))
        .add_system_set(SystemSet::on_exit(AppState::Menu).with_system(clear_stage))
        .add_system_set(
            SystemSet::on_update(AppState::Playing)
                .with_system(update_attacks)
                .with_system(enemy_actions)
                .with_system(control_player)
                .with_system(move_on_grid)
                .with_system(do_damage)
                .with_system(play_four_to_the_floor)
                .with_system(set_grid_transform)
                .with_system(check_for_player_death)
                .after("spawn_world"),
        )
        .add_system_set(
            SystemSet::on_update(AppState::Dead)
                .with_system(back_to_menu)
                .after("spawn_world"),
        )
        .add_system_set(SystemSet::on_enter(AppState::Dead).with_system(player_died))
        .add_system_set(SystemSet::on_exit(AppState::Dead).with_system(clear_stage))
        .add_startup_system(setup)
        .run();
}

const NORMAL_BUTTON: Color = Color::rgb(0.15, 0.15, 0.15);
const HOVERED_BUTTON: Color = Color::rgb(0.25, 0.25, 0.25);
const PRESSED_BUTTON: Color = Color::rgb(0.55, 0.55, 0.55);

fn handle_button(
    mut interaction_query: Query<
        (&Interaction, &mut UiColor),
        (Changed<Interaction>, With<Button>),
    >,
    mut app_state: ResMut<State<AppState>>,
) {
    for (interaction, mut color) in interaction_query.iter_mut() {
        match *interaction {
            Interaction::Clicked => {
                *color = PRESSED_BUTTON.into();
                app_state.set(AppState::Playing).unwrap();
            }
            Interaction::Hovered => {
                *color = HOVERED_BUTTON.into();
            }
            Interaction::None => {
                *color = NORMAL_BUTTON.into();
            }
        }
    }
}

fn spawn_menu(
    mut commands: Commands,
    mut scene_spawner: ResMut<SceneSpawner>,
    asset_server: Res<AssetServer>,
) {
    commands
        .spawn_bundle(NodeBundle {
            style: Style {
                size: Size::new(Val::Percent(100.0), Val::Percent(100.0)),
                justify_content: JustifyContent::SpaceBetween,
                ..Default::default()
            },
            color: Color::NONE.into(),
            ..Default::default()
        })
        .insert(SceneRoot)
        .with_children(|commands| {
            commands
                .spawn_bundle(ButtonBundle {
                    style: Style {
                        size: Size::new(Val::Px(300.0), Val::Px(70.0)),
                        // center button
                        margin: Rect::all(Val::Auto),
                        // horizontally center child text
                        justify_content: JustifyContent::Center,
                        // vertically center child text
                        align_items: AlignItems::Center,
                        ..Default::default()
                    },
                    color: NORMAL_BUTTON.into(),
                    ..Default::default()
                })
                .with_children(|parent| {
                    parent.spawn_bundle(TextBundle {
                        text: Text::with_section(
                            "LOS GEHT'S",
                            TextStyle {
                                font: asset_server.load("fonts/Gamer.ttf"),
                                font_size: 40.0,
                                color: Color::rgb(0.9, 0.9, 0.9),
                            },
                            Default::default(),
                        ),
                        ..Default::default()
                    });
                });
        });

    let root_3d = commands
        .spawn_bundle(PbrBundle::default())
        .insert(SceneRoot)
        .insert(Children::default())
        .id();
    scene_spawner.spawn_as_child(asset_server.load("berghain.glb#Scene0"), root_3d);
}

fn do_damage(
    mut attacks: Query<&mut AttackComponent>,
    attackers: Query<&Team>,
    mut entities: Query<(Entity, &GridPosition, &mut Health, &mut Alive, &Team)>,
) {
    for mut atk in attacks.iter_mut() {
        let attacker_team = attackers.get(atk.owner).unwrap();
        for (e, pos, mut health, mut alive, team) in entities.iter_mut() {
            if *team != *attacker_team && atk.already_hit.insert(e) {
                if let Some(atk_data) = atk.attack.hit(pos) {
                    health.amount = health.amount.saturating_sub(atk_data.damage);
                    if health.amount == 0 {
                        alive.alive = false;
                    }
                }
            }
        }
    }
}

fn check_for_player_death(mut app_state: ResMut<State<AppState>>, players: Query<&IsPlayer>) {
    if players.is_empty() {
        app_state.set(AppState::Dead).unwrap();
    }
}
#[derive(Component)]
struct DeathText;

fn player_died(
    mut commands: Commands,
    attacks: Query<(Entity, &AttackComponent)>,
    asset_server: Res<AssetServer>,
) {
    for (e, _) in attacks.iter() {
        commands.entity(e).despawn();
    }

    commands
        .spawn_bundle(NodeBundle {
            style: Style {
                size: Size::new(Val::Percent(100.0), Val::Percent(100.0)),
                justify_content: JustifyContent::SpaceBetween,
                ..Default::default()
            },
            color: Color::NONE.into(),
            ..Default::default()
        })
        .insert(SceneRoot)
        .with_children(|commands| {
            commands
                .spawn_bundle(TextBundle {
                    style: Style {
                        align_self: AlignSelf::FlexEnd,
                        position_type: PositionType::Absolute,
                        position: Rect {
                            bottom: Val::Px(60.0),
                            right: Val::Px(20.0),
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    // Use the `Text::with_section` constructor
                    text: Text::with_section(
                        // Accepts a `String` or any type that converts into a `String`, such as `&str`
                        "The beats are over",
                        TextStyle {
                            font: asset_server.load("fonts/edunline.ttf"),
                            font_size: 100.0,
                            color: Color::WHITE,
                        },
                        // Note: You can use `Default::default()` in place of the `TextAlignment`
                        TextAlignment {
                            horizontal: HorizontalAlign::Center,
                            ..Default::default()
                        },
                    ),
                    ..Default::default()
                })
                .insert(DeathText);
            commands
                .spawn_bundle(TextBundle {
                    style: Style {
                        align_self: AlignSelf::FlexEnd,
                        position_type: PositionType::Absolute,
                        position: Rect {
                            bottom: Val::Px(30.0),
                            right: Val::Px(20.0),
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    // Use the `Text::with_section` constructor
                    text: Text::with_section(
                        // Accepts a `String` or any type that converts into a `String`, such as `&str`
                        "Press any key to return to the menu",
                        TextStyle {
                            font: asset_server.load("fonts/Gamer.ttf"),
                            font_size: 30.0,
                            color: Color::WHITE,
                        },
                        // Note: You can use `Default::default()` in place of the `TextAlignment`
                        TextAlignment {
                            horizontal: HorizontalAlign::Center,
                            ..Default::default()
                        },
                    ),
                    ..Default::default()
                })
                .insert(DeathText);
        });
}

fn update_attacks(
    mut beats: EventReader<BeatEvent>,
    mut attacks: Query<(&mut AttackComponent, &mut Alive)>,
) {
    for beat in beats.iter() {
        for (mut atk, mut alive) in attacks.iter_mut() {
            alive.alive &= atk.attack.update(beat);
        }
    }
}

fn enemy_actions(
    mut commands: Commands,
    mut beats: EventReader<BeatEvent>,
    grids: Query<&Grid>,
    targets: Query<&GridPosition>,
    mut enemies: Query<(
        Entity,
        &mut Enemy,
        Option<&Target>,
        &GridPosition,
        &mut GridMovement,
        &Parent,
    )>,
    audio: Res<Audio>,
) {
    for beat in beats.iter() {
        for (entity, mut enemy, target, pos, mut mvmt, parent) in enemies.iter_mut() {
            if enemy.attack_pattern.interval == *beat {
                if let Some(action) = enemy.attack_pattern.pattern.next() {
                    if let Some(atk) = action.attack {
                        audio.play(enemy.attack_pattern.sound.clone());
                        let grid = grids.get(parent.0.clone()).unwrap();
                        let atk_cmp =
                            atk.attack(&mut *enemy.attack_pattern.state, entity, grid, pos);
                        commands
                            .spawn()
                            .insert(atk_cmp)
                            .insert(Alive { alive: true })
                            .insert(Parent(parent.0.clone()));
                    }

                    if let Some(mover) = action.mover {
                        let grid = grids.get(parent.0.clone()).unwrap();

                        let target = target.map(|t| targets.get(t.0.clone()).unwrap());
                        let state = &mut *enemy.attack_pattern.state;

                        if let Some(dir) = mover.choose_move(state, target, grid, pos) {
                            *mvmt = dir;
                        }
                    }
                }
            }
        }
    }
}

fn set_grid_attack_colors(
    mut elements: Query<(&GridPosition, &mut GridElement, &Parent)>,
    attacks: Query<(&AttackComponent, &Parent)>,
) {
    for (_, mut element, _) in elements.iter_mut() {
        element.color = DEFAULT_GRID_COLOR;
    }

    for (pos, mut element, el_parent) in elements.iter_mut() {
        for (atk, atk_parent) in attacks.iter() {
            if el_parent == atk_parent && atk.attack.hit(pos).is_some() {
                element.color = DANGER_GRID_COLOR;
                break;
            }
        }
    }
}

fn play_four_to_the_floor(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    audio: Res<Audio>,
    mut events: EventReader<BeatEvent>,
    mut elements: Query<(Entity, &Handle<StandardMaterial>, &GridElement)>,
) {
    if events.iter().any(|&beat| beat == BeatEvent::Quarter) {
        audio.play(asset_server.load("kick-hit.wav"));
        for (e, _, _) in elements.iter_mut() {
            commands
                .entity(e)
                .insert(Animator::new(Tween::<GridElement>::new(
                    EaseFunction::QuadraticIn,
                    TweeningType::Once,
                    Duration::from_secs_f32(0.3),
                    GridLightnessLens {
                        start: 1.,
                        end: 0.8,
                    },
                )));
        }
    }
}

fn set_grid_transform(
    grids: Query<&Grid>,
    mut objs: Query<(&Parent, &GridPosition, &mut Transform)>,
) {
    for (parent, grid_pos, mut trans) in objs.iter_mut() {
        let grid = grids.get(parent.0).unwrap();

        let mut pos = Vec3::new(grid_pos.x as f32, 0., grid_pos.y as f32);

        pos *= grid.size;

        trans.translation = pos;
    }
}

fn back_to_menu(mut input: EventReader<KeyboardInput>, mut state: ResMut<State<AppState>>) {
    for i in input.iter() {
        if i.state == ElementState::Pressed {
            state.set(AppState::Menu).unwrap();
        }
    }
}

fn control_player(
    mut input: EventReader<KeyboardInput>,
    mut objs: Query<(&mut GridMovement, &IsPlayer)>,
) {
    for i in input.iter() {
        let dir = match i {
            KeyboardInput {
                key_code: Some(KeyCode::Left),
                state: ElementState::Pressed,
                ..
            } => (-1, 0),
            KeyboardInput {
                key_code: Some(KeyCode::Right),
                state: ElementState::Pressed,
                ..
            } => (1, 0),
            KeyboardInput {
                key_code: Some(KeyCode::Up),
                state: ElementState::Pressed,
                ..
            } => (0, -1),
            KeyboardInput {
                key_code: Some(KeyCode::Down),
                state: ElementState::Pressed,
                ..
            } => (0, 1),
            _ => continue,
        };

        for (mut mvmt, _) in objs.iter_mut() {
            mvmt.x = dir.0;
            mvmt.y = dir.1;
        }

        // We only process one player input per frame, limiting us to cardinal directions.
        break;
    }
}

fn move_on_grid(
    grids: Query<&Grid>,
    mut objs: Query<(
        &mut GridPosition,
        &mut GridMovement,
        Option<&mut Transform>,
        &Parent,
    )>,
) {
    for (mut grid_pos, mut mvmt, trans, parent) in objs.iter_mut() {
        let grid = grids.get(parent.0).unwrap();

        if grid.can_move(*grid_pos, *mvmt) {
            *grid_pos += *mvmt;

            if let Some(mut trans) = trans {
                if mvmt.x != 0 || mvmt.y != 0 {
                    trans.rotation = Quat::from_euler(
                        EulerRot::YXZ,
                        (mvmt.x as f32).atan2(mvmt.y as f32),
                        0.,
                        0.,
                    );
                }
            }
        }

        *mvmt = Default::default();
    }
}

fn spawn_enemy<'a, 'w, 's>(
    commands: &'a mut Commands<'w, 's>,
    materials: &mut Assets<StandardMaterial>,
    mesh: Handle<Mesh>,
    texture: Handle<Image>,
    attack_pattern: PatternPlayer,
) -> Entity {
    commands
        .spawn_bundle(PbrBundle {
            mesh,
            material: materials.add(StandardMaterial {
                base_color_texture: Some(texture),
                perceptual_roughness: 0.2,
                ..Default::default()
            }),
            transform: Transform::identity().with_scale(Vec3::splat(4.)),
            ..Default::default()
        })
        .insert(Team::Enemy)
        .insert(Enemy { attack_pattern })
        .insert(Health { amount: 1 })
        .insert(GridMovement::default())
        .id()
}

fn spawn_hat(
    commands: &mut Commands,
    materials: &mut Assets<StandardMaterial>,
    asset_server: &AssetServer,
    skip: usize,
) -> Entity {
    const MAX_DIST: u8 = 2;

    struct MakeHatAttack;
    struct HatMover;

    struct HatAttack {
        pos: GridPosition,
        dist: u8,
    }

    struct HatMoveState {
        desired_dir: GridMovement,
    }

    impl Mover for HatMover {
        fn choose_move(
            &self,
            state: &mut dyn Any,
            _target: Option<&GridPosition>,
            grid: &Grid,
            pos: &GridPosition,
        ) -> Option<GridMovement> {
            let state: &mut HatMoveState = state.downcast_mut().unwrap();

            if !grid.can_move(*pos, state.desired_dir) {
                state.desired_dir = -state.desired_dir;
            }

            Some(state.desired_dir)
        }
    }

    impl Attack for HatAttack {
        fn hit(&self, pos: &GridPosition) -> Option<AttackData> {
            if ((Some(pos.x) == self.pos.x.checked_sub(self.dist)
                || pos.x == self.pos.x + self.dist)
                && (self.pos.y.saturating_sub(self.dist)..=self.pos.y + self.dist).contains(&pos.y))
                || ((Some(pos.y) == self.pos.y.checked_sub(self.dist)
                    || pos.y == self.pos.y + self.dist)
                    && (self.pos.x.saturating_sub(self.dist)..=self.pos.x + self.dist)
                        .contains(&pos.x))
            {
                Some(AttackData { damage: 1 })
            } else {
                None
            }
        }

        fn update(&mut self, beat: &BeatEvent) -> bool {
            match beat {
                BeatEvent::EighthTriplet => {
                    self.dist += 1;
                }
                _ => {}
            }

            self.dist <= MAX_DIST
        }
    }

    impl MakeAttack for MakeHatAttack {
        fn attack(
            &self,
            _state: &mut dyn Any,
            owner: Entity,
            _grid: &Grid,
            pos: &GridPosition,
        ) -> AttackComponent {
            AttackComponent {
                owner,
                already_hit: Default::default(),
                attack: Box::new(HatAttack { pos: *pos, dist: 1 }),
            }
        }
    }

    const MAKE_ATTACK: &dyn MakeAttack = &MakeHatAttack;
    const MOVER: &dyn Mover = &HatMover;
    const ATTACK_PATTERN: &[PatternStep] = &[
        PatternStep::mover(MOVER),
        PatternStep::mover(MOVER),
        PatternStep::mover(MOVER),
        PatternStep::mover(MOVER),
        PatternStep::mover(MOVER),
        PatternStep::mover(MOVER),
        PatternStep::mover(MOVER),
        PatternStep {
            attack: Some(MAKE_ATTACK),
            mover: Some(MOVER),
        },
    ];

    let mut pattern = ATTACK_PATTERN.iter().cycle();

    for _ in 0..skip {
        pattern.next();
        pattern.next();
    }

    spawn_enemy(
        commands,
        materials,
        asset_server.load("enemy.1.obj"),
        asset_server.load("enemy.1.png"),
        PatternPlayer {
            state: Box::new(HatMoveState {
                desired_dir: GridMovement { x: 1, y: 0 },
            }),
            sound: asset_server.load("hat-hit.wav"),
            pattern,
            interval: BeatEvent::Eighth,
        },
    )
}

fn spawn_snare(
    commands: &mut Commands,
    materials: &mut Assets<StandardMaterial>,
    asset_server: &AssetServer,
    skip: usize,
    dir: GridMovement,
) -> Entity {
    struct MakeSnareAttack;
    struct SnareMover;
    struct SnareAttack {
        pos: GridPosition,
        dist_x: RangeInclusive<u8>,
        dist_y: RangeInclusive<u8>,
    }
    struct SnareMoveState {
        desired_dir: GridMovement,
    }

    impl Mover for SnareMover {
        fn choose_move(
            &self,
            state: &mut dyn Any,
            _target: Option<&GridPosition>,
            grid: &Grid,
            pos: &GridPosition,
        ) -> Option<GridMovement> {
            let state: &mut SnareMoveState = state.downcast_mut().unwrap();

            if !grid.can_move(*pos, state.desired_dir) {
                state.desired_dir = -state.desired_dir;
            }

            Some(state.desired_dir)
        }
    }

    impl Attack for SnareAttack {
        fn hit(&self, pos: &GridPosition) -> Option<AttackData> {
            if (self.pos.x == pos.x && self.dist_y.contains(&pos.y))
                || (self.pos.y == pos.y && self.dist_x.contains(&pos.x))
            {
                Some(AttackData { damage: 1 })
            } else {
                None
            }
        }

        fn update(&mut self, beat: &BeatEvent) -> bool {
            match beat {
                BeatEvent::Quarter => false,
                _ => true,
            }
        }
    }

    impl MakeAttack for MakeSnareAttack {
        fn attack(
            &self,
            _state: &mut dyn Any,
            owner: Entity,
            grid: &Grid,
            pos: &GridPosition,
        ) -> AttackComponent {
            fn calculate_extent(grid: &Grid, mut pos: GridPosition, dir: GridMovement) -> u8 {
                let mut extent = 0;

                while grid.can_move(pos, dir) {
                    extent += 1;
                    pos += dir;
                }

                extent
            }

            let (l, r, u, d) = (
                calculate_extent(grid, *pos, GridMovement { x: -1, y: 0 }),
                calculate_extent(grid, *pos, GridMovement { x: 1, y: 0 }),
                calculate_extent(grid, *pos, GridMovement { x: 0, y: -1 }),
                calculate_extent(grid, *pos, GridMovement { x: 0, y: 1 }),
            );
            AttackComponent {
                owner,
                already_hit: Default::default(),
                attack: Box::new(SnareAttack {
                    dist_x: pos.x - l..=pos.x + r,
                    dist_y: pos.y - u..=pos.y + d,
                    pos: *pos,
                }),
            }
        }
    }

    const MAKE_ATTACK: &dyn MakeAttack = &MakeSnareAttack;
    const MOVER: &dyn Mover = &SnareMover;
    const ATTACK_PATTERN: &[PatternStep] = &[
        PatternStep::mover(MOVER),
        PatternStep::mover(MOVER),
        PatternStep::mover(MOVER),
        PatternStep::attack(MAKE_ATTACK),
    ];

    let mut pattern = ATTACK_PATTERN.iter().cycle();

    for _ in 0..skip {
        pattern.next();
    }

    spawn_enemy(
        commands,
        materials,
        asset_server.load("enemy.0.obj"),
        asset_server.load("enemy.0.png"),
        PatternPlayer {
            state: Box::new(SnareMoveState { desired_dir: dir }),
            sound: asset_server.load("snare-hit.wav"),
            pattern,
            interval: BeatEvent::Quarter,
        },
    )
}

struct GridLightnessLens {
    start: f32,
    end: f32,
}

impl Lens<GridElement> for GridLightnessLens {
    fn lerp(&mut self, target: &mut GridElement, ratio: f32) {
        let Self { start, end } = *self;
        let calc = start + (end - start) * ratio;

        target.lightness = calc;
    }
}

fn remove_dead(mut commands: Commands, elements: Query<(Entity, &Alive)>) {
    for (e, alive) in elements.iter() {
        if !alive.alive {
            commands.entity(e).despawn_recursive();
        }
    }
}

fn update_grid_elements(
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut elements: Query<(Entity, &Handle<StandardMaterial>, &GridElement)>,
) {
    fn mul_color(color: &Color, amt: f32) -> Color {
        match *color {
            Color::Rgba {
                red,
                green,
                blue,
                alpha,
            } => Color::rgba(red * amt, green * amt, blue * amt, alpha),
            Color::RgbaLinear {
                red,
                green,
                blue,
                alpha,
            } => Color::rgba_linear(red * amt, green * amt, blue * amt, alpha),
            Color::Hsla {
                hue,
                saturation,
                lightness,
                alpha,
            } => Color::hsla(hue, saturation, lightness * amt, alpha),
        }
    }

    for (_, mat, grid) in elements.iter_mut() {
        materials.get_mut(mat.clone()).unwrap().base_color = mul_color(&grid.color, grid.lightness);
    }
}

fn start_playing(
    mut commands: Commands,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut meshes: ResMut<Assets<Mesh>>,
    asset_server: Res<AssetServer>,
    mut scene_spawner: ResMut<SceneSpawner>,
) {
    let root = commands
        .spawn()
        .insert_bundle(PbrBundle::default())
        .insert(SceneRoot)
        .insert(Children::with(&[]))
        .id();

    let grid_size = 5.;
    let grid_shape = {
        let (x, o) = (Collision::Solid, Collision::NonSolid);
        vec![
            vec![o, o, o, o, o, o, x, x, x, x, o, o, o, o, o, o],
            vec![o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o],
            vec![o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o],
            vec![o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o],
            vec![x, o, o, o, o, o, o, o, o, o, o, o, o, o, o, x],
            vec![x, o, o, o, o, o, o, o, o, o, o, o, o, o, o, x],
            vec![x, o, o, o, o, o, o, o, o, o, o, o, o, o, o, x],
            vec![o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o],
            vec![o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o],
            vec![o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o],
            vec![o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o],
            vec![o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o],
            vec![o, o, o, x, x, x, o, o, o, o, x, x, x, o, o, o],
            vec![o, o, o, x, x, x, o, o, o, o, x, x, x, o, o, o],
            vec![o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o],
        ]
    };

    let grid_element_mesh = meshes.add(
        bevy::render::mesh::shape::Plane {
            size: grid_size - 1.,
        }
        .into(),
    );
    let element_color = DEFAULT_GRID_COLOR;
    let grid = commands
        .spawn()
        .insert_bundle(PbrBundle {
            transform: Transform::identity().with_translation(Vec3::new(-18., 41., -78.)),
            ..Default::default()
        })
        .insert(Grid {
            shape: grid_shape.clone(),
            size: grid_size,
        })
        .insert(Parent(root))
        .id();

    scene_spawner.spawn_as_child(asset_server.load("berghain.glb#Scene0"), root);

    for (y, row) in grid_shape.iter().enumerate() {
        for (x, element) in row.iter().enumerate() {
            if *element == Collision::NonSolid {
                commands
                    .spawn_bundle(PbrBundle {
                        mesh: grid_element_mesh.clone(),
                        material: materials.add(StandardMaterial {
                            base_color: element_color,
                            reflectance: 0.,
                            perceptual_roughness: 0.7,
                            ..Default::default()
                        }),
                        ..Default::default()
                    })
                    .insert(GridElement {
                        lightness: 1.,
                        color: element_color,
                    })
                    .insert(GridPosition {
                        x: x as _,
                        y: y as _,
                    })
                    .insert(Parent(grid));
            }
        }
    }

    let player = commands
        .spawn_bundle(PbrBundle {
            mesh: asset_server.load("player.obj"),
            material: materials.add(StandardMaterial {
                base_color_texture: Some(asset_server.load("player.png")),
                perceptual_roughness: 0.2,
                ..Default::default()
            }),
            transform: Transform::identity().with_scale(Vec3::splat(4.)),
            ..Default::default()
        })
        .insert(Team::Player)
        .insert(GridPosition::default())
        .insert(GridMovement::default())
        .insert(IsPlayer)
        .insert(Alive { alive: true })
        .insert(Health { amount: 5 })
        .insert(Parent(grid))
        .id();

    let snare = spawn_snare(
        &mut commands,
        &mut *materials,
        &*asset_server,
        0,
        GridMovement { x: 0, y: -1 },
    );
    commands
        .entity(snare)
        .insert(Target(player))
        .insert(Parent(grid.clone()))
        .insert(GridPosition { x: 5, y: 10 });
    let snare = spawn_snare(
        &mut commands,
        &mut *materials,
        &*asset_server,
        2,
        GridMovement { x: 0, y: 1 },
    );
    commands
        .entity(snare)
        .insert(Target(player))
        .insert(Parent(grid.clone()))
        .insert(GridPosition { x: 10, y: 5 });

    for i in 0..4 {
        let hat = spawn_hat(&mut commands, &mut *materials, &*asset_server, i);
        commands
            .entity(hat)
            .insert(Target(player))
            .insert(Parent(grid.clone()))
            .insert(GridPosition {
                x: 2 + (i as u8 * 4),
                y: 1 + (i as u8 * 3),
            });
    }
}

fn clear_stage(mut commands: Commands, roots: Query<(Entity, &SceneRoot)>) {
    for (root, _) in roots.iter() {
        commands.entity(root.clone()).despawn_recursive();
    }
}

fn setup(
    mut commands: Commands,
    mut ambient_light: ResMut<AmbientLight>,
    mut timeline: ResMut<TimelineSettings>,
) {
    commands.spawn_bundle(UiCameraBundle::default());

    ambient_light.color = Color::WHITE;
    ambient_light.brightness = 0.5;

    timeline.bpm = DEFAULT_BPM as _;
    timeline.state = TimelineState::Playing;
}
