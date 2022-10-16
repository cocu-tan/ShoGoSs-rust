use std::collections::{HashMap, HashSet};
use std::fmt::format;
use std::fs::{File, read};
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::str::FromStr;

use clap::Parser;

use crate::MovableCondition::{IfEmptyAndInitialPawn, IfEnemy, Normal};

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
enum PieceType {
    // Sho-gi
    Kin,
    Gin,
    Keima,
    Kyosha { never_moved: bool },

    // Go
    Go,

    // Chess
    Pawn,
    Knight,
    Rook,
    Bishop,
    Queen,

    // Kings
    KingOu { never_moved: bool },
    SuperKingOu,
}

impl PieceType {
    fn can_move_horizontally(&self) -> bool {
        match self {
            PieceType::Rook | PieceType::Queen => true,
            _ => false,
        }
    }
    fn can_move_forward(&self) -> bool {
        match self {
            PieceType::Rook | PieceType::Queen | PieceType::Kyosha { .. } => true,
            _ => false,
        }
    }
    fn can_move_backward(&self) -> bool {
        match self {
            PieceType::Rook | PieceType::Queen => true,
            _ => false,
        }
    }
    fn can_move_diagonally(&self) -> bool {
        match self {
            PieceType::Queen | PieceType::Bishop => true,
            _ => false,
        }
    }
    fn is_king(&self) -> bool {
        match self {
            PieceType::KingOu { .. } | PieceType::SuperKingOu => true,
            _ => false,
        }
    }
}

impl FromStr for Piece {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let player = match s.chars().nth(0).expect("Not empty string").is_uppercase() {
            true => Player::White,
            false => Player::Black,
        };
        let piece_type = match s.to_lowercase().as_str() {
            "ki" => PieceType::Kin,
            "gi" => PieceType::Gin,
            "ke" => PieceType::Keima,
            "ky" => PieceType::Kyosha { never_moved: false },
            "go" => PieceType::Go,
            "pa" => PieceType::Pawn,
            "kn" => PieceType::Knight,
            "ro" => PieceType::Rook,
            "bi" => PieceType::Bishop,
            "qu" => PieceType::Queen,
            "ko" => PieceType::KingOu { never_moved: false },
            "sk" => PieceType::SuperKingOu,
            _ => return Err(format!("unexpected piece type: {:?}", s)),
        };
        return Ok(Piece::new(player, piece_type));
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
enum Player {
    Black,
    White,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
struct Piece {
    player: Player,
    piece: PieceType,
}

impl Piece {
    fn new(player: Player, piece: PieceType) -> Self {
        return Piece { player, piece };
    }
}

type XCoord = i8;
type YCoord = i8;

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug, Default)]
struct Coord {
    x: XCoord,
    y: YCoord,
}

type CoordFrom = Coord;
type CoordTo = Coord;

impl Coord {
    fn new(x: XCoord, y: YCoord) -> Self {
        return Coord { x, y };
    }

    fn is_in_board(&self) -> bool {
        let x = self.x;
        let y = self.y;
        return 0 <= x && x < 9 && 0 <= y && y < 9;
    }

    fn apply_move(&mut self, direction: &(XCoord, YCoord)) {
        self.x += direction.0;
        self.y += direction.1;
    }

    fn add_relative_offset(&self, offset: &(XCoord, YCoord), player: &Player) -> Coord {
        match player {
            Player::Black => Coord::new(self.x + offset.0, self.y - offset.1),
            Player::White => Coord::new(self.x + offset.0, self.y + offset.1),
        }
    }
}

struct Board {
    pieces: HashMap<Coord, Piece>,
}

impl Board {
    fn new(pieces: HashMap<Coord, Piece>) -> Self {
        return Board { pieces };
    }
}

enum MovableCondition {
    // The cell is empty or has a piece owned by the other user.
    Normal,

    // No pieces exist on the cell. 
    IfEmptyAndInitialPawn,

    IfEnemy,
}

impl MovableCondition {
    fn is_movable(&self, coord_from: &CoordFrom, coord_to: &CoordTo, current_player: &Player,
                  board: &Board, pawn_counts: &HashMap<Player, HashMap<XCoord, HashSet<Coord>>>) -> bool {
        match self {
            Normal => {
                return board.pieces.get(coord_to)
                    .map(|x| x.player.ne(current_player))
                    .unwrap_or(true);
            }
            IfEmptyAndInitialPawn => {
                let is_initial_pawn = match current_player {
                    Player::Black => coord_from.y == 6,
                    Player::White => coord_from.y == 2,
                };

                return board.pieces.get(coord_to).is_none() && is_initial_pawn;
            }
            IfEnemy => {
                let cannot_be_two_pawn = pawn_counts.get(current_player).expect("No player in pawn count").get(&coord_to.x).map(|set| set.len()).unwrap_or(0) == 0;
                return board.pieces.get(coord_to)
                    .map(|x| x.player.ne(current_player))
                    .unwrap_or(false)
                    && cannot_be_two_pawn;
            }
        }
    }
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
enum LoseCause {
    TwoPawn {
        coords: Vec<Coord>,
    },
    CheckMate {
        coord_from: CoordFrom,
        coord_king: CoordTo,
    },
    Tori {
        coord_king: Coord,
    },
}

impl LoseCause {
    fn is_potentially_retrievable_in_next_turn(&self) -> bool {
        match self {
            LoseCause::TwoPawn { .. } => false,
            LoseCause::CheckMate { .. } => true,
            LoseCause::Tori { .. } => false,
        }
    }
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
struct LoseReason {
    player: Player,
    cause: LoseCause,
}

impl LoseReason {
    fn new(player: Player, cause: LoseCause) -> Self {
        LoseReason { player, cause }
    }
}

struct Judgement {
    board: Board,
}

impl Judgement {
    pub fn new(board: Board) -> Self {
        return Judgement {
            board,
        };
    }

    // Returns tsumi players
    pub fn check(&self) -> Vec<LoseReason> {
        let mut result: Vec<LoseReason> = Vec::new();

        let pawn_counts: HashMap<Player, HashMap<XCoord, HashSet<Coord>>> = gen_pawn_counts(&self.board);

        // Checks two pawn.
        for (player, counts) in pawn_counts.iter() {
            for (_, coords) in counts.iter() {
                if coords.len() > 1 {
                    result.push(LoseReason::new(player.clone(), LoseCause::TwoPawn { coords: coords.iter().cloned().collect() }));
                }
            }
        }

        // Gens reachable table
        let reachable_table: HashMap<CoordFrom, HashSet<CoordTo>> = gen_reachable_table(&self.board, &pawn_counts);
        let mut inverse_reachable_table: HashMap<CoordTo, HashSet<CoordFrom>> = HashMap::new();
        for (c_from, cs_to) in reachable_table.iter() {
            for c_to in cs_to {
                inverse_reachable_table.entry(c_to.clone()).or_default().insert(c_from.clone());
            }
        }

        // Checks checkmates
        let kings: Vec<(&Coord, &Piece)> = self.board.pieces.iter().filter(|(_, p)| p.piece.is_king()).collect();
        for (c_king, p_king) in kings.iter() {
            for c_from in inverse_reachable_table.get(c_king).unwrap_or(&HashSet::new()).iter() {
                let p_from = self.board.pieces.get(c_from).expect("as iterating inv reachable map");
                if p_from.player.ne(&p_king.player) {
                    // Player A is reachable to Player B's king => Checkmate.
                    result.push(LoseReason::new(p_king.player, LoseCause::CheckMate { coord_from: c_from.clone(), coord_king: **c_king }))
                }
            }
        }

        // Checks tori.
        for (c_king, p_king) in kings.iter() {
            if (is_kakoware(&self.board, c_king)) {
                result.push(LoseReason::new(p_king.player, LoseCause::Tori { coord_king: **c_king }));
            }
        }


        result
    }
}

fn is_kakoware(board: &Board, c_base: &Coord) -> bool {
    let mut queue: Vec<Coord> = Vec::new();
    let mut checked: HashSet<Coord> = HashSet::new();
    let p_base = board.pieces.get(&c_base);
    if p_base.is_none() { return false; }
    let current_player = &p_base.unwrap().player;

    queue.push(c_base.clone());
    while let Some(c_target) = queue.pop() {
        if checked.contains(&c_target) {
            continue;
        }

        if let Some(p_target) = board.pieces.get(&c_target) {
            // The cell has a piece
            if p_target.player.eq(current_player) {
                // Owned by current user. => Keep investigating
                queue.extend(get_neighbours(&c_target));
            } else {
                // Owned by Enemy. => Stop investigation as potentially a part of kakoware
            }
        } else {
            // Exists a cell has no piece. => Not Kakoware.
            return false;
        }
        checked.insert(c_target);
    }
    true
}

fn get_neighbours(coord: &Coord) -> Vec<Coord> {
    return vec![
        (-1, 0),
        (1, 0),
        (0, -1),
        (0, 1),
    ].iter().map(|offset| {
        let mut new_c = coord.clone();
        new_c.apply_move(&offset);
        new_c
    }).filter(|c| c.is_in_board()).collect();
}

fn gen_pawn_counts(board: &Board) -> HashMap<Player, HashMap<XCoord, HashSet<Coord>>> {
    let mut pawn_count_for_x: HashMap<Player, HashMap<XCoord, HashSet<Coord>>> = HashMap::new();
    for (c, p) in board.pieces.iter().filter(|(_, p)| p.piece == PieceType::Pawn) {
        pawn_count_for_x.entry(p.player).or_default().entry(c.x).or_default().insert(c.clone());
    };
    return pawn_count_for_x;
}

fn gen_reachable_table(board: &Board, pawn_counts: &HashMap<Player, HashMap<XCoord, HashSet<Coord>>>) -> HashMap<CoordFrom, HashSet<CoordTo>> {
    let mut reachable_table: HashMap<CoordFrom, HashSet<CoordTo>> = HashMap::new();

    for (coord_from, cell_from) in board.pieces.iter() {
        if cell_from.piece.eq(&PieceType::SuperKingOu) {
            // SuperKingOu will be calculated in the last.
            continue;
        }

        let mut reachables: HashSet<Coord> = _get_fixed_relative_reachable(cell_from.piece).iter()
            .flat_map(|(offset, cond)| {
                let coord_to = coord_from.add_relative_offset(&offset, &cell_from.player);
                if !coord_to.is_in_board() {
                    None
                } else if cond.is_movable(&coord_from, &coord_to, &cell_from.player, board, pawn_counts) {
                    Some(coord_to)
                } else {
                    None
                }
            }).collect();

        let mut directions: Vec<(XCoord, YCoord)> = vec![];
        if cell_from.piece.can_move_diagonally() {
            directions.push((-1, -1));
            directions.push((1, -1));
            directions.push((-1, 1));
            directions.push((1, 1));
        }
        if cell_from.piece.can_move_horizontally() {
            directions.push((-1, 0));
            directions.push((1, 0));
        }
        if cell_from.piece.can_move_forward() {
            directions.push((0, 1));
        }
        if cell_from.piece.can_move_backward() {
            directions.push((0, -1));
        }
        for direction in directions {
            let mut coord_to = coord_from.clone();
            coord_to.apply_move(&direction);
            while coord_to.is_in_board() {
                let cell_to = board.pieces.get(&coord_to);
                if (cell_to.is_some()) {
                    // Target cell is not empty
                    // Can reach if the cell is enemy.
                    if cell_from.player.ne(&cell_to.unwrap().player) {
                        reachables.insert(coord_to.clone());
                    }
                    break;
                }

                // Target cell is empty, continue searching
                reachables.insert(coord_to.clone());
                coord_to.apply_move(&direction);
            }
        }

        reachables.into_iter().for_each(|coord_to| {
            reachable_table.entry(coord_from.clone()).or_default().insert(coord_to.clone());
        });
    }


    // Super King Ou
    let coords_super_king_ou: Vec<Coord> = board.pieces.iter().filter(|(_, p)| p.piece.eq(&PieceType::SuperKingOu)).map(|(c, _)| c).cloned().collect();
    if !coords_super_king_ou.is_empty() {
        let all_reachable_cells: HashSet<Coord> = reachable_table.values().flat_map(|s| s).cloned().collect();
        let non_reachable_cells: HashSet<Coord> =
            (0..9).flat_map(|y| (0..9).map(move |x| Coord::new(x, y)))
                .filter(|c| !all_reachable_cells.contains(c))
                .collect();
        coords_super_king_ou.iter().for_each(|c| reachable_table
            .entry(*c).or_default().extend(non_reachable_cells.clone()));
    }


    return reachable_table;
}


fn _is_empty_cell(s: &String) -> bool {
    s == "--"
}

fn parse_board<S>(inp: Vec<Vec<S>>) -> Board where S: Into<String> {
    let mut b: HashMap<Coord, Piece> = HashMap::new();
    for (y, row) in inp.into_iter().enumerate()
    {
        for (x, piece_raw) in row.into_iter().enumerate() {
            let piece = piece_raw.into();
            if _is_empty_cell(&piece) { continue; }
            let c = Coord::new(x as i8, y as i8);

            b.insert(c, Piece::from_str(&piece).expect("no failure in parse"));
        }
    }
    Board::new(b)
}


fn _get_fixed_relative_reachable(pt: PieceType) -> Vec<((XCoord, YCoord), MovableCondition)> {
    return
        match pt {
            PieceType::Kin => vec![
                ((-1, 1), Normal), ((0, 1), Normal), ((1, 1), Normal),
                ((-1, 0), Normal), ((1, 0), Normal), ((0, -1), Normal),
            ],
            PieceType::Gin => vec![
                ((-1, 1), Normal), ((0, 1), Normal), ((1, 1), Normal),
                ((-1, -1), Normal), ((1, -1), Normal),
            ],
            PieceType::Keima => vec![
                ((-1, 2), Normal), ((1, 2), Normal),
            ],
            PieceType::Kyosha { .. } => vec![],//checks in absolute offsets
            PieceType::Go => vec![],
            PieceType::Pawn => vec![
                ((0, 1), Normal),
                ((-1, 1), IfEnemy),
                ((1, 1), IfEnemy),
                ((0, 2), IfEmptyAndInitialPawn),
            ],
            PieceType::Knight => vec![
                ((-2, 1), Normal),
                ((-1, 2), Normal),
                ((1, 2), Normal),
                ((2, 1), Normal),
                ((-2, -1), Normal),
                ((-1, -2), Normal),
                ((1, -2), Normal),
                ((2, -1), Normal),
            ],
            PieceType::Rook => vec![],//checks in absolute offsets
            PieceType::Bishop => vec![],//checks in absolute offsets
            PieceType::Queen => vec![],//checks in absolute offsets
            PieceType::KingOu { .. } => vec![
                ((-1, -1), Normal),
                ((0, -1), Normal),
                ((1, -1), Normal),
                ((-1, 0), Normal),
                ((1, 0), Normal),
                ((-1, 1), Normal),
                ((0, 1), Normal),
                ((1, 1), Normal),
            ],
            PieceType::SuperKingOu => vec![],//checks in absolute offsets
        };
}


#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Path to ShoGoSs board text file
    path: PathBuf,
}

fn main() {
    let args: Args = Args::parse();

    let path: BufReader<_> = BufReader::new(File::open(args.path).unwrap());

    let mut board_str: Vec<Vec<String>> = Vec::new();
    for line in path.lines() {
        let string = line.expect("np to read string from stdin").to_string();
        if string.trim().is_empty() {
            continue;
        }
        let row: Vec<String> = string.split(",").map(|s| s.to_string().replace("\n", "")).collect();
        assert!(row.len() == 9, "Each row must have 9 elements");
        board_str.push(row);
    };
    assert!(board_str.len() == 9, "{:?}", board_str);
    let board = parse_board(board_str);
    let judgement = Judgement::new(board);
    let results = judgement.check();

    if results.is_empty() {
        println!("Judgement result: Not finished yet.");
    } else {
        println!("Judgement result:");
        for result in results {
            println!("{:?}", result)
        }
    }
}


// Not supported yet:
//  - Kumaring
//  - Casting

// +------->x
// | WHITE
// |
// | black
// v
macro_rules! board {
    ($b: expr)  => {
        crate::parse_board($b)
    };
}

#[cfg(test)]
mod tests__check_two_pawn {
    use std::str::FromStr;

    use crate::{Coord, Judgement, LoseCause, LoseReason, Player};

    #[test]
    fn empty__no_tsumi() {
        let b = board!(vec![
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
        ]);
        assert!(Judgement::new(b).check().is_empty());
    }

    #[test]
    fn initial__no_lose() {
        let b = board!(vec![
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["Pa","Pa","Pa","Pa","Pa","Pa","Pa","Pa","Pa"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["pa","pa","pa","pa","pa","pa","pa","pa","pa"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
        ]);
        assert!(Judgement::new(b).check().is_empty());
    }

    #[test]
    fn white_nipo__white_lose() {
        let b = board!(vec![
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","Pa","--","--","--","--","--","--","--"],
            vec!["Pa","Pa","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","Pa","--","--","Pa","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","Pa","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
        ]);
        assert!(Judgement::new(b).check() == vec![LoseReason::new(Player::White, LoseCause::TwoPawn)]);
    }

    #[test]
    fn black_nipo__black_lose() {
        let b = board!(vec![
            vec!["--","--","--","--","--","pa","--","--","--"],
            vec!["--","Pa","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","pa","--","--","--"],
            vec!["--","pa","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
        ]);
        assert!(Judgement::new(b).check() == vec![LoseReason::new(Player::Black, LoseCause::TwoPawn)]);
    }

    #[test]
    fn checkmate__direct_pawn() {
        let b = board!(vec![
            vec!["--","--","--","--","KO","--","--","--","--"],
            vec!["--","--","--","--","pa","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
        ]);
        let result = Judgement::new(b).check();
        assert!(result.len() == 1);
        assert!(result.get(0).unwrap().cause == LoseCause::CheckMate { coord_from: Coord::new(4, 1), coord_king: Coord::new(4, 0) }, "{:?}", result)
    }

    #[test]
    fn checkmate__rook() {
        let b = board!(vec![
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","ro","--","KO","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
        ]);
        let result = Judgement::new(b).check();
        assert!(result.len() == 1);
        assert!(result.get(0).unwrap().cause ==
                    LoseCause::CheckMate { coord_from: Coord::new(2, 3), coord_king: Coord::new(4, 3) }, "{:?}", result)
    }


    #[test]
    fn kakoi_complete() {
        let b = board!(vec![
            vec!["--","--","--","go","GO","go","--","--","--"],
            vec!["--","--","go","GO","GO","go","--","--","--"],
            vec!["--","--","go","GO","GO","go","GO","--","--"],
            vec!["--","--","ro","GO","KO","go","--","--","--"],
            vec!["--","--","--","go","go","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
        ]);
        let result = Judgement::new(b).check();
        assert!(result.len() == 1);
        assert!(result.get(0).unwrap().cause ==
                    LoseCause::Tori { coord_king: Coord::new(4, 3) }, "{:?}", result)
    }


    #[test]
    fn kakoi_incomplete() {
        let b = board!(vec![
            vec!["--","--","--","go","GO","go","--","--","--"],
            vec!["--","--","go","GO","GO","go","--","--","--"],
            vec!["--","--","go","GO","GO","go","GO","--","--"],
            vec!["--","--","ro","GO","KO","go","--","--","--"],
            vec!["--","--","--","--","go","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
        ]);
        let result = Judgement::new(b).check();
        assert!(result.is_empty());
    }
}


#[cfg(test)]
mod tests__reachable_table {
    use std::str::FromStr;

    use crate::{Coord, gen_pawn_counts, gen_reachable_table, Judgement, LoseCause, LoseReason, Player};

    #[test]
    fn empty__returns_empty() {
        let b = board!(vec![
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
        ]);

        let c = gen_pawn_counts(&b);
        let t = gen_reachable_table(&b, &c);

        assert!(t.is_empty());
    }


    #[test]
    fn one_initial_pawn__returns_two() {
        let b = board!(vec![
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["pa","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
        ]);

        let c = gen_pawn_counts(&b);
        let t = gen_reachable_table(&b, &c);

        assert!(t.contains_key(&Coord::new(0, 6)), "{:?}", t);
        assert!(t.get(&Coord::new(0, 6)).unwrap().len() == 2, "{:?}", t);
    }


    #[test]
    fn one_initial_pawn_with_blocker__returns_one() {
        let b = board!(vec![
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["PA","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["pa","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
        ]);

        let c = gen_pawn_counts(&b);
        let t = gen_reachable_table(&b, &c);

        assert!(t.contains_key(&Coord::new(0, 6)), "{:?}", t);
        assert!(t.get(&Coord::new(0, 6)).unwrap().len() == 1, "{:?}", t);
    }

    #[test]
    fn pawn_with_enemy__returns_two() {
        let b = board!(vec![
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","PA","--","--","--","--","--","--","--"],
            vec!["pa","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
        ]);

        let c = gen_pawn_counts(&b);
        let t = gen_reachable_table(&b, &c);

        assert!(t.contains_key(&Coord::new(0, 4)), "{:?}", t);
        assert!(t.get(&Coord::new(0, 4)).unwrap().len() == 2, "{:?}", t);
    }

    #[test]
    fn pawn_with_nipoable_enemy__returns_one() {
        let b = board!(vec![
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","PA","--","--","--","--","--","--","--"],
            vec!["pa","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","pa","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
        ]);

        let c = gen_pawn_counts(&b);
        let t = gen_reachable_table(&b, &c);

        assert!(t.contains_key(&Coord::new(0, 4)), "{:?}", t);
        assert!(t.get(&Coord::new(0, 4)).unwrap().len() == 1, "{:?}", t);
    }


    #[test]
    fn queen() {
        let b = board!(vec![
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["pa","--","--","--","pa","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","qu","--","pa","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","pa","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
        ]);

        let c = gen_pawn_counts(&b);
        let t = gen_reachable_table(&b, &c);

        assert!(t.contains_key(&Coord::new(2, 4)), "{:?}", t);
        assert!(t.get(&Coord::new(2, 4)).unwrap().len() == 17, "t.get(&Coord::new(2, 4)).unwrap().len()={:?}\n{:?}", t.get(&Coord::new(2, 4)).unwrap().len(), t);
    }

    #[test]
    fn rook() {
        let b = board!(vec![
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","pa","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","ro","--","KO","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","pa","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
            vec!["--","--","--","--","--","--","--","--","--"],
        ]);

        let c = gen_pawn_counts(&b);
        let t = gen_reachable_table(&b, &c);

        let value = t.get(&Coord::new(2, 3));

        assert!(value.is_some(), "{:?}", value);
        assert!(value.unwrap().len() == 6, "t.get(&Coord::new(2, 4)).unwrap().len()={:?}\n{:?}", value, t);
    }
}