# しょ碁ス (ShoGoSs) ソルバー

## Features

- 盤面からどっちが勝っているかを出力する

## Usage

```shell
$ git clone
$ cd ShoGoSs-rust


$ cat example_input/nipo
--,--,--,--,--,pa,--,--,--
--,PA,--,--,--,--,--,--,--
--,--,--,--,--,--,--,--,--
--,--,--,--,--,--,--,--,--
--,--,--,--,--,pa,--,--,--
--,pa,--,--,--,--,--,--,--
--,--,--,--,--,--,--,--,--
--,--,--,--,--,--,--,--,--
--,--,--,--,--,--,--,--,--
$ cargo run example_input/nipo
Judgement result:
LoseReason { player: Black, cause: TwoPawn { coords: [Coord { x: 5, y: 0 }, Coord { x: 5, y: 4 }] } }


$ cat example_input/kakoi
--,go,GO,GO,go,go,--,--,--
go,go,GO,GO,GO,go,--,--,--
GO,GO,GO,GO,GO,go,--,--,--
go,go,GO,KO,GO,go,--,--,--
--,go,GO,GO,GO,go,--,--,--
--,pa,go,go,go,--,--,--,--
--,--,--,--,--,--,--,--,--
--,--,--,--,--,--,--,--,--
--,--,--,--,--,--,--,--,--
$ cargo run example_input/kakoi
Judgement result:
LoseReason { player: White, cause: Tori { coord_king: Coord { x: 3, y: 3 } } }

```

### Input

- Player:
    - Large case: White player
    - Smaller case: Black player
- Piece format

```rust
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
```

```text
--,--,--,--,--,pa,--,--,--
--,PA,--,--,--,--,--,--,--
--,--,--,--,--,--,--,--,--
--,--,--,--,--,--,--,--,--
--,--,--,--,--,pa,--,--,--
--,pa,--,--,--,--,--,--,--
--,--,--,--,--,--,--,--,--
--,--,--,--,--,--,--,--,--
--,--,--,--,--,--,--,--,--
```