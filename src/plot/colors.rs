use bevy::prelude::*;

use std::collections::HashMap;

#[derive(Resource, Default)]
pub struct ResourceHashMap<K, V> {
    pub hashmap: HashMap<K, V>,
}
impl<K, V> ResourceHashMap<K, V> {
    pub fn new() -> ResourceHashMap<K, V> {
        ResourceHashMap {
            hashmap: HashMap::new(),
        }
    }
    pub fn new_with_hashmap(map: HashMap<K, V>) -> ResourceHashMap<K, V> {
        ResourceHashMap { hashmap: map }
    }
}

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum PlotColor {
    Gray,
    Black,
    LightPink,
    Pink,
    Violet,
    Blue,
    Green,
    Salmon,
    Orange,
    Latte,
    Cream,
    Yellow,
}

/// To get a particular color, get the color from the hashmap with a key of the PlotColor enum.
/// Then get the shade of this color from the Vec of colors, the higher the index the darker the shade.
pub fn make_color_palette() -> ResourceHashMap<PlotColor, Vec<Color>> {
    let gray = ["d4d2dd", "b4b3b9", "aaa9b1", "9f9ea4", "66656a", "59585e"]
        .iter()
        .map(
            // to hex
            |h| Color::hex(h).unwrap(),
        )
        .collect::<Vec<Color>>();

    let black = ["38373c", "323337", "49484d", "323136", "1c1c1c", "111111"]
        .iter()
        .map(
            // to hex
            |h| Color::hex(h).unwrap(),
        )
        .collect::<Vec<Color>>();

    let light_pink = ["f1b8bf", "d08693", "ecbbbf", "f2b9bf", "febdc5", "df9ea6"]
        .iter()
        .map(
            // to hex
            |h| Color::hex(h).unwrap(),
        )
        .collect::<Vec<Color>>();

    let pink = ["f05285", "f9558a", "e74479", "f85187", "e9467d", "ca1950"]
        .iter()
        .map(
            // to hex
            |h| Color::hex(h).unwrap(),
        )
        .collect::<Vec<Color>>();

    let violet = ["9e6ea2", "94639a", "64356c", "9d71a2", "714576", "4b2451"]
        .iter()
        .map(
            // to hex
            |h| Color::hex(h).unwrap(),
        )
        .collect::<Vec<Color>>();

    let blue = ["5197ca", "4a8dc1", "4285ba", "226599", "3b6d90", "1c567e"]
        .iter()
        .map(
            // to hex
            |h| Color::hex(h).unwrap(),
        )
        .collect::<Vec<Color>>();

    let green = ["afce92", "a2c986", "b6dd9a", "8eb274", "8eb274", "366821"]
        .iter()
        .map(
            // to hex
            |h| Color::hex(h).unwrap(),
        )
        .collect::<Vec<Color>>();

    let salmon = ["f96960", "e6564d", "fc655e", "df4442", "dc4846", "bb2727"]
        .iter()
        .map(
            // to hex
            |h| Color::hex(h).unwrap(),
        )
        .collect::<Vec<Color>>();

    let orange = ["f8ae6d", "ffaf6a", "e78347", "f28e50", "e16f3b", "cb6229"]
        .iter()
        .map(
            // to hex
            |h| Color::hex(h).unwrap(),
        )
        .collect::<Vec<Color>>();

    let latte = ["dbb993", "e5c49b", "dbbb92", "d1ae86", "be9b71", "b38e62"]
        .iter()
        .map(
            // to hex
            |h| Color::hex(h).unwrap(),
        )
        .collect::<Vec<Color>>();

    let cream = ["f7efe4", "f6edde", "f5e9d9", "f2e6d8", "e9dbce", "e8dccc"]
        .iter()
        .map(
            // to hex
            |h| Color::hex(h).unwrap(),
        )
        .collect::<Vec<Color>>();

    let yellow = ["fcd402", "fcd305", "fad008", "efc000", "f9c907", "d8a600"]
        .iter()
        .map(
            // to hex
            |h| Color::hex(h).unwrap(),
        )
        .collect::<Vec<Color>>();

    let mut color_map = ResourceHashMap::new();

    let colors = &mut color_map.hashmap;

    colors.insert(PlotColor::Gray, gray);
    colors.insert(PlotColor::Black, black);
    colors.insert(PlotColor::LightPink, light_pink);
    colors.insert(PlotColor::Pink, pink);
    colors.insert(PlotColor::Violet, violet);
    colors.insert(PlotColor::Blue, blue);
    colors.insert(PlotColor::Green, green);
    colors.insert(PlotColor::Salmon, salmon);
    colors.insert(PlotColor::Orange, orange);
    colors.insert(PlotColor::Latte, latte);
    colors.insert(PlotColor::Cream, cream);
    colors.insert(PlotColor::Yellow, yellow);

    color_map
}
