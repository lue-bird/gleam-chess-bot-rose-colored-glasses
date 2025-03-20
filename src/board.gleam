import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import gleam/string
import gleam/string_tree

pub type Bitboard =
  Int

pub fn new_bitboard(bitboard: Int) -> Bitboard {
  bitboard
}

pub fn empty_bitboard() -> Bitboard {
  0
}

pub fn full_bitboard() -> Bitboard {
  0xffffffffffffffff
}

pub fn bitboard_and(bitboard1: Bitboard, bitboard2: Bitboard) -> Bitboard {
  int.bitwise_and(bitboard1, bitboard2)
}

pub fn bitboard_exclusive_or(
  bitboard1: Bitboard,
  bitboard2: Bitboard,
) -> Bitboard {
  int.bitwise_exclusive_or(bitboard1, bitboard2)
}

pub fn bitboard_or(bitboard1: Bitboard, bitboard2: Bitboard) -> Bitboard {
  int.bitwise_or(bitboard1, bitboard2)
}

pub fn bitboard_not(bitboard: Bitboard) -> Bitboard {
  int.bitwise_not(bitboard)
}

pub fn bitboard_shift_left(bitboard: Bitboard, shift: Int) -> Bitboard {
  int.bitwise_shift_left(bitboard, shift)
}

pub fn bitboard_shift_right(bitboard: Bitboard, shift: Int) -> Bitboard {
  int.bitwise_shift_right(bitboard, shift)
}

pub fn bitboard_bitscan_forward(bitboard: Bitboard) -> Int {
  bitboard_bitscan_forward_inner(bitboard, 0)
}

pub fn bitboard_bitscan_forward_inner(bitboard: Bitboard, index: Int) -> Int {
  case bitboard == 0 || index > 63 {
    True -> -1
    False -> {
      let lsb_digit =
        0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000001
      let first_digit_bitboard = bitboard_and(bitboard, new_bitboard(lsb_digit))
      case first_digit_bitboard {
        0 ->
          bitboard_bitscan_forward_inner(
            bitboard_shift_right(bitboard, 1),
            index + 1,
          )
        _ -> index
      }
    }
  }
}

pub fn bitscan_backward(bitboard: Bitboard) -> Int {
  bitscan_backward_inner(bitboard, 63)
}

pub fn bitscan_backward_inner(bitboard: Bitboard, index: Int) -> Int {
  case bitboard == 0 || index < 0 {
    True -> -1
    False -> {
      let msb_digit =
        0b10000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000
      let first_digit_bitboard = bitboard_and(bitboard, new_bitboard(msb_digit))
      case first_digit_bitboard {
        0 -> bitscan_backward_inner(bitboard_shift_left(bitboard, 1), index - 1)
        _ -> index
      }
    }
  }
}

pub type BoardBB {
  BoardBB(
    black_king_bitboard: Bitboard,
    black_queen_bitboard: Bitboard,
    black_rook_bitboard: Bitboard,
    black_bishop_bitboard: Bitboard,
    black_knight_bitboard: Bitboard,
    black_pawns_bitboard: Bitboard,
    white_king_bitboard: Bitboard,
    white_queen_bitboard: Bitboard,
    white_rook_bitboard: Bitboard,
    white_bishop_bitboard: Bitboard,
    white_knight_bitboard: Bitboard,
    white_pawns_bitboard: Bitboard,
  )
}

pub type Color {
  White
  Black
}

pub fn color_to_string(color: Color) -> String {
  case color {
    White -> "White"
    Black -> "Black"
  }
}

pub type Piece {
  Piece(color: Color, kind: PieceKind)
}

pub type PieceKind {
  Pawn
  Knight
  Bishop
  Rook
  Queen
  King
}

pub fn piece_to_string(piece: Piece) -> String {
  let kind = case piece.kind {
    Pawn -> "Pawn"
    Knight -> "Knight"
    Bishop -> "Bishop"
    Rook -> "Rook"
    Queen -> "Queen"
    King -> "King"
  }

  let color = case piece.color {
    White -> color_to_string(White)
    Black -> color_to_string(Black)
  }

  color <> " " <> kind
}

pub type Position {
  Position(file: File, rank: Rank)
}

pub type File {
  A
  B
  C
  D
  E
  F
  G
  H
}

pub type Rank {
  One
  Two
  Three
  Four
  Five
  Six
  Seven
  Eight
}

pub fn are_the_same(position1: Position, position2: Position) -> Bool {
  position1.file == position2.file && position1.rank == position2.rank
}

pub fn distance_between(position1: Position, position2: Position) -> Int {
  let pos1_as_int = position_to_int(position1)
  let pos2_as_int = position_to_int(position2)
  let distance = pos1_as_int - pos2_as_int
  distance
}

pub fn position_to_int(position: Position) -> Int {
  let file = file_to_int(position.file)
  let rank = rank_to_int(position.rank)
  let pos_as_int = file + { rank * 8 }
  pos_as_int
}

pub fn to_string(position: Position) -> String {
  let file = file_to_string(position.file)
  let rank = rank_to_string(position.rank)
  let pos_as_string = file <> rank
  pos_as_string
}

pub fn file_to_string(file: File) -> String {
  case file {
    A -> "a"
    B -> "b"
    C -> "c"
    D -> "d"
    E -> "e"
    F -> "f"
    G -> "g"
    H -> "h"
  }
}

pub fn rank_to_string(rank: Rank) -> String {
  case rank {
    One -> "1"
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
  }
}

pub fn file_to_int(file: File) -> Int {
  case file {
    A -> 0
    B -> 1
    C -> 2
    D -> 3
    E -> 4
    F -> 5
    G -> 6
    H -> 7
  }
}

pub fn rank_to_int(rank: Rank) -> Int {
  case rank {
    One -> 0
    Two -> 1
    Three -> 2
    Four -> 3
    Five -> 4
    Six -> 5
    Seven -> 6
    Eight -> 7
  }
}

pub fn position_from_int(i: Int) -> Result(Position, _) {
  case i {
    i if i >= 0 && i < 64 -> {
      let file = int_to_file(i % 8)
      let rank = int_to_rank(i / 8)
      Ok(Position(file, rank))
    }
    _ -> Error("Invalid position")
  }
}

// should return option or result
pub fn int_to_rank(i: Int) -> Rank {
  case i {
    0 -> One
    1 -> Two
    2 -> Three
    3 -> Four
    4 -> Five
    5 -> Six
    6 -> Seven
    7 -> Eight
    _ -> panic
  }
}

// should return option or result
pub fn int_to_file(i: Int) -> File {
  case i {
    0 -> A
    1 -> B
    2 -> C
    3 -> D
    4 -> E
    5 -> F
    6 -> G
    7 -> H
    _ -> panic
  }
}

//a function that returns a position that is x squares rank-wise away from the given position and y squares file-wise away from the given position
pub fn get_position(position: Position, x: Int, y: Int) -> Position {
  let file = get_file(position.file, y)
  let rank = get_rank(position.rank, x)
  Position(file, rank)
}

pub fn get_position_relative(
  position: Position,
  x: Int,
  y: Int,
  relative_to: Color,
) -> Position {
  let file = get_file_relative(position.file, y, relative_to)
  let rank = get_rank_relative(position.rank, x, relative_to)
  Position(file, rank)
}

pub fn get_file_relative(file: File, y: Int, relative_to: Color) -> File {
  case relative_to {
    White -> get_file(file, y)
    Black -> get_file(file, -y)
  }
}

pub fn get_rank_relative(rank: Rank, x: Int, relative_to: Color) -> Rank {
  case relative_to {
    White -> get_rank(rank, x)
    Black -> get_rank(rank, -x)
  }
}

pub fn get_file(file: File, y: Int) -> File {
  let file_as_int = file_to_int(file)
  let new_file_as_int = file_as_int + y
  let new_file = int_to_file(new_file_as_int)
  new_file
}

pub fn get_rank(rank: Rank, x: Int) -> Rank {
  let rank_as_int = rank_to_int(rank)
  let new_rank_as_int = rank_as_int + x
  let new_rank = int_to_rank(new_rank_as_int)
  new_rank
}

pub fn get_rear_position(position: Position, color: Color) -> Position {
  let rank = get_rear_rank(position.rank, color)
  Position(position.file, rank)
}

pub fn get_rear_rank(rank: Rank, color: Color) -> Rank {
  case color {
    White -> get_rear_white_rank(rank)
    Black -> get_rear_black_rank(rank)
  }
}

pub fn get_rear_white_rank(rank: Rank) -> Rank {
  case rank {
    One -> One
    Two -> One
    Three -> Two
    Four -> Three
    Five -> Four
    Six -> Five
    Seven -> Six
    Eight -> Seven
  }
}

pub fn get_rear_black_rank(rank: Rank) -> Rank {
  case rank {
    One -> Two
    Two -> Three
    Three -> Four
    Four -> Five
    Five -> Six
    Six -> Seven
    Seven -> Eight
    Eight -> Eight
  }
}

pub fn remove_piece_at_position(
  board: BoardBB,
  position: Position,
) -> Result(BoardBB, _) {
  let bitboard = bitboard_not(from_position(position))

  let new_board = case get_piece_at_position(board, position) {
    None -> Error("No piece at position")
    Some(Piece(color: color, kind: kind)) if color == White && kind == King -> {
      Ok(
        BoardBB(
          ..board,
          white_king_bitboard: bitboard_and(bitboard, board.white_king_bitboard),
        ),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == White && kind == Queen -> {
      Ok(
        BoardBB(
          ..board,
          white_queen_bitboard: bitboard_and(
            bitboard,
            board.white_queen_bitboard,
          ),
        ),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == White && kind == Rook -> {
      Ok(
        BoardBB(
          ..board,
          white_rook_bitboard: bitboard_and(bitboard, board.white_rook_bitboard),
        ),
      )
    }

    Some(Piece(color: color, kind: kind)) if color == White && kind == Bishop -> {
      Ok(
        BoardBB(
          ..board,
          white_bishop_bitboard: bitboard_and(
            bitboard,
            board.white_bishop_bitboard,
          ),
        ),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == White && kind == Knight -> {
      Ok(
        BoardBB(
          ..board,
          white_knight_bitboard: bitboard_and(
            bitboard,
            board.white_knight_bitboard,
          ),
        ),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == White && kind == Pawn -> {
      Ok(
        BoardBB(
          ..board,
          white_pawns_bitboard: bitboard_and(
            bitboard,
            board.white_pawns_bitboard,
          ),
        ),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == Black && kind == King -> {
      Ok(
        BoardBB(
          ..board,
          black_king_bitboard: bitboard_and(bitboard, board.black_king_bitboard),
        ),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == Black && kind == Queen -> {
      Ok(
        BoardBB(
          ..board,
          black_queen_bitboard: bitboard_and(
            bitboard,
            board.black_queen_bitboard,
          ),
        ),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == Black && kind == Rook -> {
      Ok(
        BoardBB(
          ..board,
          black_rook_bitboard: bitboard_and(bitboard, board.black_rook_bitboard),
        ),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == Black && kind == Bishop -> {
      Ok(
        BoardBB(
          ..board,
          black_bishop_bitboard: bitboard_and(
            bitboard,
            board.black_bishop_bitboard,
          ),
        ),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == Black && kind == Knight -> {
      Ok(
        BoardBB(
          ..board,
          black_knight_bitboard: bitboard_and(
            bitboard,
            board.black_knight_bitboard,
          ),
        ),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == Black && kind == Pawn -> {
      Ok(
        BoardBB(
          ..board,
          black_pawns_bitboard: bitboard_and(
            bitboard,
            board.black_pawns_bitboard,
          ),
        ),
      )
    }
    _ -> {
      panic as "Invalid piece"
    }
  }
  new_board
}

pub fn set_piece_at_position(
  board: BoardBB,
  position: Position,
  piece: Piece,
) -> BoardBB {
  let bitboard = from_position(position)

  let new_board = case piece {
    Piece(color: color, kind: kind) if color == White && kind == King -> {
      BoardBB(
        ..board,
        white_king_bitboard: bitboard_or(bitboard, board.white_king_bitboard),
      )
    }
    Piece(color: color, kind: kind) if color == White && kind == Queen -> {
      BoardBB(
        ..board,
        white_queen_bitboard: bitboard_or(bitboard, board.white_queen_bitboard),
      )
    }
    Piece(color: color, kind: kind) if color == White && kind == Rook -> {
      BoardBB(
        ..board,
        white_rook_bitboard: bitboard_or(bitboard, board.white_rook_bitboard),
      )
    }
    Piece(color: color, kind: kind) if color == White && kind == Bishop -> {
      BoardBB(
        ..board,
        white_bishop_bitboard: bitboard_or(
          bitboard,
          board.white_bishop_bitboard,
        ),
      )
    }
    Piece(color: color, kind: kind) if color == White && kind == Knight -> {
      BoardBB(
        ..board,
        white_knight_bitboard: bitboard_or(
          bitboard,
          board.white_knight_bitboard,
        ),
      )
    }
    Piece(color: color, kind: kind) if color == White && kind == Pawn -> {
      BoardBB(
        ..board,
        white_pawns_bitboard: bitboard_or(bitboard, board.white_pawns_bitboard),
      )
    }
    Piece(color: color, kind: kind) if color == Black && kind == King -> {
      BoardBB(
        ..board,
        black_king_bitboard: bitboard_or(bitboard, board.black_king_bitboard),
      )
    }
    Piece(color: color, kind: kind) if color == Black && kind == Queen -> {
      BoardBB(
        ..board,
        black_queen_bitboard: bitboard_or(bitboard, board.black_queen_bitboard),
      )
    }
    Piece(color: color, kind: kind) if color == Black && kind == Rook -> {
      BoardBB(
        ..board,
        black_rook_bitboard: bitboard_or(bitboard, board.black_rook_bitboard),
      )
    }
    Piece(color: color, kind: kind) if color == Black && kind == Bishop -> {
      BoardBB(
        ..board,
        black_bishop_bitboard: bitboard_or(
          bitboard,
          board.black_bishop_bitboard,
        ),
      )
    }
    Piece(color: color, kind: kind) if color == Black && kind == Knight -> {
      BoardBB(
        ..board,
        black_knight_bitboard: bitboard_or(
          bitboard,
          board.black_knight_bitboard,
        ),
      )
    }
    Piece(color: color, kind: kind) if color == Black && kind == Pawn -> {
      BoardBB(
        ..board,
        black_pawns_bitboard: bitboard_or(bitboard, board.black_pawns_bitboard),
      )
    }
    _ -> {
      panic as "Invalid piece"
    }
  }
  new_board
}

pub fn get_piece_at_position(board: BoardBB, position: Position) {
  let bitboard = from_position(position)
  let black_king_bb_compare = bitboard_and(board.black_king_bitboard, bitboard)
  let black_queen_bb_compare =
    bitboard_and(board.black_queen_bitboard, bitboard)
  let black_rook_bb_compare = bitboard_and(board.black_rook_bitboard, bitboard)
  let black_bishop_bb_compare =
    bitboard_and(board.black_bishop_bitboard, bitboard)
  let black_knight_bb_compare =
    bitboard_and(board.black_knight_bitboard, bitboard)
  let black_pawns_bb_compare =
    bitboard_and(board.black_pawns_bitboard, bitboard)
  let white_king_bb_compare = bitboard_and(board.white_king_bitboard, bitboard)
  let white_queen_bb_compare =
    bitboard_and(board.white_queen_bitboard, bitboard)
  let white_rook_bb_compare = bitboard_and(board.white_rook_bitboard, bitboard)
  let white_bishop_bb_compare =
    bitboard_and(board.white_bishop_bitboard, bitboard)
  let white_knight_bb_compare =
    bitboard_and(board.white_knight_bitboard, bitboard)
  let white_pawns_bb_compare =
    bitboard_and(board.white_pawns_bitboard, bitboard)

  let piece = case bitboard {
    0 -> None
    _ -> {
      let piece = case bitboard {
        _ if bitboard == black_king_bb_compare -> Some(Piece(Black, King))
        _ if bitboard == black_queen_bb_compare -> Some(Piece(Black, Queen))
        _ if bitboard == black_rook_bb_compare -> Some(Piece(Black, Rook))
        _ if bitboard == black_bishop_bb_compare -> Some(Piece(Black, Bishop))
        _ if bitboard == black_knight_bb_compare -> Some(Piece(Black, Knight))
        _ if bitboard == black_pawns_bb_compare -> Some(Piece(Black, Pawn))
        _ if bitboard == white_king_bb_compare -> Some(Piece(White, King))
        _ if bitboard == white_queen_bb_compare -> Some(Piece(White, Queen))
        _ if bitboard == white_rook_bb_compare -> Some(Piece(White, Rook))
        _ if bitboard == white_bishop_bb_compare -> Some(Piece(White, Bishop))
        _ if bitboard == white_knight_bb_compare -> Some(Piece(White, Knight))
        _ if bitboard == white_pawns_bb_compare -> Some(Piece(White, Pawn))
        _ -> None
      }
      piece
    }
  }
  piece
}

pub fn get_all_positions(board: BoardBB) -> Result(List(Position), _) {
  let list_of_bitboards = [
    board.black_king_bitboard,
    board.black_queen_bitboard,
    board.black_rook_bitboard,
    board.black_bishop_bitboard,
    board.black_knight_bitboard,
    board.black_pawns_bitboard,
    board.white_king_bitboard,
    board.white_queen_bitboard,
    board.white_rook_bitboard,
    board.white_bishop_bitboard,
    board.white_knight_bitboard,
    board.white_pawns_bitboard,
  ]

  let positions =
    list.fold(list_of_bitboards, set.new(), fn(acc, bitboard) {
      let positions = case get_positions(bitboard) {
        Ok(positions) -> positions
        Error(_) -> []
      }
      let positions = set.from_list(positions)
      set.union(acc, positions)
    })

  Ok(set.to_list(positions))
}

pub fn get_positions(bitboard: Bitboard) -> Result(List(Position), _) {
  let positions = []
  case bitboard {
    0 -> Ok(positions)
    _ -> {
      let count = 63
      let just_first_bit_of_bb =
        bitboard_and(bitboard, new_bitboard(0x8000000000000000))
      case just_first_bit_of_bb {
        0 -> get_positions_inner(bitboard_shift_left(bitboard, 1), count - 1)
        _ -> {
          use position_dest <- result.try(position_from_int(count))
          use positions_inner <- result.try(get_positions_inner(
            bitboard_shift_left(bitboard, 1),
            count - 1,
          ))
          Ok([position_dest, ..positions_inner])
        }
      }
    }
  }
}

pub fn get_positions_inner(
  bitboard: Bitboard,
  count: Int,
) -> Result(List(Position), _) {
  case count < 0 {
    True -> Ok([])
    False -> {
      let just_first_bit_of_bb =
        bitboard_and(bitboard, new_bitboard(0x8000000000000000))
      case just_first_bit_of_bb {
        0 -> get_positions_inner(bitboard_shift_left(bitboard, 1), count - 1)
        _ -> {
          use position_dest <- result.try(position_from_int(count))
          use positions_inner <- result.try(get_positions_inner(
            bitboard_shift_left(bitboard, 1),
            count - 1,
          ))
          Ok([position_dest, ..positions_inner])
        }
      }
    }
  }
}

pub fn from_position(position: Position) -> Bitboard {
  let bitboard = bitboard_shift_left(new_bitboard(1), position_to_int(position))
  bitboard
}

pub type MoveSan {
  MoveSanNormal(
    from: Option(PositionSan),
    to: Position,
    moving_piece: PieceKind,
    capture: Bool,
    promotion: Option(PieceKind),
    maybe_check_or_checkmate: Option(CheckOrCheckMate),
  )
  MoveSanCastle(
    side: CastleSide,
    maybe_check_or_checkmate: Option(CheckOrCheckMate),
  )
  MoveSanEnPassant(
    from: Option(PositionSan),
    to: Position,
    maybe_check_or_checkmate: Option(CheckOrCheckMate),
  )
}

pub type PositionSan {
  PositionSan(file: Option(File), rank: Option(Rank))
}

pub type ErrorSan {
  InvalidMoveString
  InvalidCastleString
  InvalidPositionalInformation
}

pub type CastleSide {
  KingSide
  QueenSide
}

pub type CheckOrCheckMate {
  Check
  CheckMate
}

// FEN

pub type CastlingStatus {
  CastlingStatus(
    white_kingside: Bool,
    white_queenside: Bool,
    black_kingside: Bool,
    black_queenside: Bool,
  )
}

pub type HalfMove =
  Int

pub type FullMove =
  Int

pub type Fen {
  Fen(
    board: BoardBB,
    turn: Color,
    castling: CastlingStatus,
    en_passant: Option(Position),
    halfmove: HalfMove,
    fullmove: FullMove,
  )
}

const positions_in_fen_order = [
  Position(file: A, rank: Eight),
  Position(file: B, rank: Eight),
  Position(file: C, rank: Eight),
  Position(file: D, rank: Eight),
  Position(file: E, rank: Eight),
  Position(file: F, rank: Eight),
  Position(file: G, rank: Eight),
  Position(file: H, rank: Eight),
  Position(file: A, rank: Seven),
  Position(file: B, rank: Seven),
  Position(file: C, rank: Seven),
  Position(file: D, rank: Seven),
  Position(file: E, rank: Seven),
  Position(file: F, rank: Seven),
  Position(file: G, rank: Seven),
  Position(file: H, rank: Seven),
  Position(file: A, rank: Six),
  Position(file: B, rank: Six),
  Position(file: C, rank: Six),
  Position(file: D, rank: Six),
  Position(file: E, rank: Six),
  Position(file: F, rank: Six),
  Position(file: G, rank: Six),
  Position(file: H, rank: Six),
  Position(file: A, rank: Five),
  Position(file: B, rank: Five),
  Position(file: C, rank: Five),
  Position(file: D, rank: Five),
  Position(file: E, rank: Five),
  Position(file: F, rank: Five),
  Position(file: G, rank: Five),
  Position(file: H, rank: Five),
  Position(file: A, rank: Four),
  Position(file: B, rank: Four),
  Position(file: C, rank: Four),
  Position(file: D, rank: Four),
  Position(file: E, rank: Four),
  Position(file: F, rank: Four),
  Position(file: G, rank: Four),
  Position(file: H, rank: Four),
  Position(file: A, rank: Three),
  Position(file: B, rank: Three),
  Position(file: C, rank: Three),
  Position(file: D, rank: Three),
  Position(file: E, rank: Three),
  Position(file: F, rank: Three),
  Position(file: G, rank: Three),
  Position(file: H, rank: Three),
  Position(file: A, rank: Two),
  Position(file: B, rank: Two),
  Position(file: C, rank: Two),
  Position(file: D, rank: Two),
  Position(file: E, rank: Two),
  Position(file: F, rank: Two),
  Position(file: G, rank: Two),
  Position(file: H, rank: Two),
  Position(file: A, rank: One),
  Position(file: B, rank: One),
  Position(file: C, rank: One),
  Position(file: D, rank: One),
  Position(file: E, rank: One),
  Position(file: F, rank: One),
  Position(file: G, rank: One),
  Position(file: H, rank: One),
]

pub fn to_board(fen: String) -> BoardBB {
  let fen_string_parts = string.split(fen, " ")
  let parsed_board = case list.length(fen_string_parts) == 6 {
    False -> panic as "Invalid FEN string"
    True -> {
      let assert [board_string, ..] = fen_string_parts
      let parsed_board = parse_board(board_string)
      parsed_board
    }
  }
  parsed_board
}

pub fn fen_from_string(fen: String) -> Result(Fen, String) {
  let fen = string.trim(fen)
  let fen_string_parts = string.split(fen, " ")
  case list.length(fen_string_parts) == 6 {
    False -> Error("Invalid FEN string")
    True -> {
      // TODO convert to Error
      let assert [board_string, ..rest] = fen_string_parts
      let assert [turn_string, ..rest] = rest
      let assert [castling_string, ..rest] = rest
      let assert [en_passant_string, ..rest] = rest
      let assert [halfmove_string, ..rest] = rest
      let assert [fullmove_string, ..] = rest

      let parsed_board = parse_board(board_string)
      let parsed_turn = parse_turn(turn_string)
      let parsed_castling = parse_castling(castling_string)
      let parsed_en_passant = parse_en_passant(en_passant_string)
      let parsed_halfmove = parse_halfmove(halfmove_string)
      let parsed_fullmove = parse_fullmove(fullmove_string)

      let fen =
        Fen(
          board: parsed_board,
          turn: parsed_turn,
          castling: parsed_castling,
          en_passant: parsed_en_passant,
          halfmove: parsed_halfmove,
          fullmove: parsed_fullmove,
        )

      Ok(fen)
    }
  }
}

// This function parses the board part of the FEN string
pub fn parse_board(board_string: String) -> BoardBB {
  // in the context of this function, rank means a an entire row of the board 
  // represented as a string of piece chars and numbers for empy spaces
  // example: "rnbqk1nr"

  let list_of_ranks_as_strings = string.split(board_string, "/")

  let accumulator =
    BoardBB(
      black_king_bitboard: 0,
      black_queen_bitboard: 0,
      black_rook_bitboard: 0,
      black_bishop_bitboard: 0,
      black_knight_bitboard: 0,
      black_pawns_bitboard: 0,
      white_king_bitboard: 0,
      white_queen_bitboard: 0,
      white_rook_bitboard: 0,
      white_bishop_bitboard: 0,
      white_knight_bitboard: 0,
      white_pawns_bitboard: 0,
    )

  list.index_fold(
    list_of_ranks_as_strings,
    accumulator,
    fn(acc, rank_as_string, rank_index) {
      let rank_index = int_to_rank(7 - rank_index)
      let rank_parts = string.to_graphemes(rank_as_string)
      let expanded_rank = expand_rank(rank_parts)
      list.index_fold(expanded_rank, acc, fn(acc, square, file_index) {
        let file_index = int_to_file(file_index)
        case square {
          "" -> acc
          "K" -> {
            let new_white_king_bitboard =
              from_position(Position(file: file_index, rank: rank_index))
            BoardBB(
              black_king_bitboard: acc.black_king_bitboard,
              black_queen_bitboard: acc.black_queen_bitboard,
              black_rook_bitboard: acc.black_rook_bitboard,
              black_bishop_bitboard: acc.black_bishop_bitboard,
              black_knight_bitboard: acc.black_knight_bitboard,
              black_pawns_bitboard: acc.black_pawns_bitboard,
              white_king_bitboard: bitboard_or(
                acc.white_king_bitboard,
                new_white_king_bitboard,
              ),
              white_queen_bitboard: acc.white_queen_bitboard,
              white_rook_bitboard: acc.white_rook_bitboard,
              white_bishop_bitboard: acc.white_bishop_bitboard,
              white_knight_bitboard: acc.white_knight_bitboard,
              white_pawns_bitboard: acc.white_pawns_bitboard,
            )
          }
          "Q" -> {
            let new_white_queen_bitboard =
              from_position(Position(file: file_index, rank: rank_index))
            BoardBB(
              black_king_bitboard: acc.black_king_bitboard,
              black_queen_bitboard: acc.black_queen_bitboard,
              black_rook_bitboard: acc.black_rook_bitboard,
              black_bishop_bitboard: acc.black_bishop_bitboard,
              black_knight_bitboard: acc.black_knight_bitboard,
              black_pawns_bitboard: acc.black_pawns_bitboard,
              white_king_bitboard: acc.white_king_bitboard,
              white_queen_bitboard: bitboard_or(
                acc.white_queen_bitboard,
                new_white_queen_bitboard,
              ),
              white_rook_bitboard: acc.white_rook_bitboard,
              white_bishop_bitboard: acc.white_bishop_bitboard,
              white_knight_bitboard: acc.white_knight_bitboard,
              white_pawns_bitboard: acc.white_pawns_bitboard,
            )
          }
          "R" -> {
            let new_white_rook_bitboard =
              from_position(Position(file: file_index, rank: rank_index))
            BoardBB(
              black_king_bitboard: acc.black_king_bitboard,
              black_queen_bitboard: acc.black_queen_bitboard,
              black_rook_bitboard: acc.black_rook_bitboard,
              black_bishop_bitboard: acc.black_bishop_bitboard,
              black_knight_bitboard: acc.black_knight_bitboard,
              black_pawns_bitboard: acc.black_pawns_bitboard,
              white_king_bitboard: acc.white_king_bitboard,
              white_queen_bitboard: acc.white_queen_bitboard,
              white_rook_bitboard: bitboard_or(
                acc.white_rook_bitboard,
                new_white_rook_bitboard,
              ),
              white_bishop_bitboard: acc.white_bishop_bitboard,
              white_knight_bitboard: acc.white_knight_bitboard,
              white_pawns_bitboard: acc.white_pawns_bitboard,
            )
          }
          "B" -> {
            let new_white_bishop_bitboard =
              from_position(Position(file: file_index, rank: rank_index))
            BoardBB(
              black_king_bitboard: acc.black_king_bitboard,
              black_queen_bitboard: acc.black_queen_bitboard,
              black_rook_bitboard: acc.black_rook_bitboard,
              black_bishop_bitboard: acc.black_bishop_bitboard,
              black_knight_bitboard: acc.black_knight_bitboard,
              black_pawns_bitboard: acc.black_pawns_bitboard,
              white_king_bitboard: acc.white_king_bitboard,
              white_queen_bitboard: acc.white_queen_bitboard,
              white_rook_bitboard: acc.white_rook_bitboard,
              white_bishop_bitboard: bitboard_or(
                acc.white_bishop_bitboard,
                new_white_bishop_bitboard,
              ),
              white_knight_bitboard: acc.white_knight_bitboard,
              white_pawns_bitboard: acc.white_pawns_bitboard,
            )
          }
          "N" -> {
            let new_white_knight_bitboard =
              from_position(Position(file: file_index, rank: rank_index))
            BoardBB(
              black_king_bitboard: acc.black_king_bitboard,
              black_queen_bitboard: acc.black_queen_bitboard,
              black_rook_bitboard: acc.black_rook_bitboard,
              black_bishop_bitboard: acc.black_bishop_bitboard,
              black_knight_bitboard: acc.black_knight_bitboard,
              black_pawns_bitboard: acc.black_pawns_bitboard,
              white_king_bitboard: acc.white_king_bitboard,
              white_queen_bitboard: acc.white_queen_bitboard,
              white_rook_bitboard: acc.white_rook_bitboard,
              white_bishop_bitboard: acc.white_bishop_bitboard,
              white_knight_bitboard: bitboard_or(
                acc.white_knight_bitboard,
                new_white_knight_bitboard,
              ),
              white_pawns_bitboard: acc.white_pawns_bitboard,
            )
          }
          "P" -> {
            let new_white_pawn_bitboard =
              from_position(Position(file: file_index, rank: rank_index))
            BoardBB(
              black_king_bitboard: acc.black_king_bitboard,
              black_queen_bitboard: acc.black_queen_bitboard,
              black_rook_bitboard: acc.black_rook_bitboard,
              black_bishop_bitboard: acc.black_bishop_bitboard,
              black_knight_bitboard: acc.black_knight_bitboard,
              black_pawns_bitboard: acc.black_pawns_bitboard,
              white_king_bitboard: acc.white_king_bitboard,
              white_queen_bitboard: acc.white_queen_bitboard,
              white_rook_bitboard: acc.white_rook_bitboard,
              white_bishop_bitboard: acc.white_bishop_bitboard,
              white_knight_bitboard: acc.white_knight_bitboard,
              white_pawns_bitboard: bitboard_or(
                acc.white_pawns_bitboard,
                new_white_pawn_bitboard,
              ),
            )
          }
          "k" -> {
            let new_black_king_bitboard =
              from_position(Position(file: file_index, rank: rank_index))
            BoardBB(
              black_king_bitboard: bitboard_or(
                acc.black_king_bitboard,
                new_black_king_bitboard,
              ),
              black_queen_bitboard: acc.black_queen_bitboard,
              black_rook_bitboard: acc.black_rook_bitboard,
              black_bishop_bitboard: acc.black_bishop_bitboard,
              black_knight_bitboard: acc.black_knight_bitboard,
              black_pawns_bitboard: acc.black_pawns_bitboard,
              white_king_bitboard: acc.white_king_bitboard,
              white_queen_bitboard: acc.white_queen_bitboard,
              white_rook_bitboard: acc.white_rook_bitboard,
              white_bishop_bitboard: acc.white_bishop_bitboard,
              white_knight_bitboard: acc.white_knight_bitboard,
              white_pawns_bitboard: acc.white_pawns_bitboard,
            )
          }
          "q" -> {
            let new_black_queen_bitboard =
              from_position(Position(file: file_index, rank: rank_index))
            BoardBB(
              black_king_bitboard: acc.black_king_bitboard,
              black_queen_bitboard: bitboard_or(
                acc.black_queen_bitboard,
                new_black_queen_bitboard,
              ),
              black_rook_bitboard: acc.black_rook_bitboard,
              black_bishop_bitboard: acc.black_bishop_bitboard,
              black_knight_bitboard: acc.black_knight_bitboard,
              black_pawns_bitboard: acc.black_pawns_bitboard,
              white_king_bitboard: acc.white_king_bitboard,
              white_queen_bitboard: acc.white_queen_bitboard,
              white_rook_bitboard: acc.white_rook_bitboard,
              white_bishop_bitboard: acc.white_bishop_bitboard,
              white_knight_bitboard: acc.white_knight_bitboard,
              white_pawns_bitboard: acc.white_pawns_bitboard,
            )
          }
          "r" -> {
            let new_black_rook_bitboard =
              from_position(Position(file: file_index, rank: rank_index))
            BoardBB(
              black_king_bitboard: acc.black_king_bitboard,
              black_queen_bitboard: acc.black_queen_bitboard,
              black_rook_bitboard: bitboard_or(
                acc.black_rook_bitboard,
                new_black_rook_bitboard,
              ),
              black_bishop_bitboard: acc.black_bishop_bitboard,
              black_knight_bitboard: acc.black_knight_bitboard,
              black_pawns_bitboard: acc.black_pawns_bitboard,
              white_king_bitboard: acc.white_king_bitboard,
              white_queen_bitboard: acc.white_queen_bitboard,
              white_rook_bitboard: acc.white_rook_bitboard,
              white_bishop_bitboard: acc.white_bishop_bitboard,
              white_knight_bitboard: acc.white_knight_bitboard,
              white_pawns_bitboard: acc.white_pawns_bitboard,
            )
          }
          "b" -> {
            let new_black_bishop_bitboard =
              from_position(Position(file: file_index, rank: rank_index))
            BoardBB(
              black_king_bitboard: acc.black_king_bitboard,
              black_queen_bitboard: acc.black_queen_bitboard,
              black_rook_bitboard: acc.black_rook_bitboard,
              black_bishop_bitboard: bitboard_or(
                acc.black_bishop_bitboard,
                new_black_bishop_bitboard,
              ),
              black_knight_bitboard: acc.black_knight_bitboard,
              black_pawns_bitboard: acc.black_pawns_bitboard,
              white_king_bitboard: acc.white_king_bitboard,
              white_queen_bitboard: acc.white_queen_bitboard,
              white_rook_bitboard: acc.white_rook_bitboard,
              white_bishop_bitboard: acc.white_bishop_bitboard,
              white_knight_bitboard: acc.white_knight_bitboard,
              white_pawns_bitboard: acc.white_pawns_bitboard,
            )
          }
          "n" -> {
            let new_black_knight_bitboard =
              from_position(Position(file: file_index, rank: rank_index))
            BoardBB(
              black_king_bitboard: acc.black_king_bitboard,
              black_queen_bitboard: acc.black_queen_bitboard,
              black_rook_bitboard: acc.black_rook_bitboard,
              black_bishop_bitboard: acc.black_bishop_bitboard,
              black_knight_bitboard: bitboard_or(
                acc.black_knight_bitboard,
                new_black_knight_bitboard,
              ),
              black_pawns_bitboard: acc.black_pawns_bitboard,
              white_king_bitboard: acc.white_king_bitboard,
              white_queen_bitboard: acc.white_queen_bitboard,
              white_rook_bitboard: acc.white_rook_bitboard,
              white_bishop_bitboard: acc.white_bishop_bitboard,
              white_knight_bitboard: acc.white_knight_bitboard,
              white_pawns_bitboard: acc.white_pawns_bitboard,
            )
          }
          "p" -> {
            let new_black_pawns_bitboard =
              from_position(Position(file: file_index, rank: rank_index))
            BoardBB(
              black_king_bitboard: acc.black_king_bitboard,
              black_queen_bitboard: acc.black_queen_bitboard,
              black_rook_bitboard: acc.black_rook_bitboard,
              black_bishop_bitboard: acc.black_bishop_bitboard,
              black_knight_bitboard: acc.black_knight_bitboard,
              black_pawns_bitboard: bitboard_or(
                acc.black_pawns_bitboard,
                new_black_pawns_bitboard,
              ),
              white_king_bitboard: acc.white_king_bitboard,
              white_queen_bitboard: acc.white_queen_bitboard,
              white_rook_bitboard: acc.white_rook_bitboard,
              white_bishop_bitboard: acc.white_bishop_bitboard,
              white_knight_bitboard: acc.white_knight_bitboard,
              white_pawns_bitboard: acc.white_pawns_bitboard,
            )
          }
          _ -> {
            panic
          }
        }
      })
    },
  )
}

fn expand_rank(rank: List(String)) -> List(String) {
  let accumulator = []
  list.fold(rank, accumulator, fn(acc, part) {
    case part {
      "1" -> list.append(acc, [""])
      "2" -> list.append(acc, ["", ""])
      "3" -> list.append(acc, ["", "", ""])
      "4" -> list.append(acc, ["", "", "", ""])
      "5" -> list.append(acc, ["", "", "", "", ""])
      "6" -> list.append(acc, ["", "", "", "", "", ""])
      "7" -> list.append(acc, ["", "", "", "", "", "", ""])
      "8" -> list.append(acc, ["", "", "", "", "", "", "", ""])
      "K" -> list.append(acc, ["K"])
      "Q" -> list.append(acc, ["Q"])
      "R" -> list.append(acc, ["R"])
      "B" -> list.append(acc, ["B"])
      "N" -> list.append(acc, ["N"])
      "P" -> list.append(acc, ["P"])
      "k" -> list.append(acc, ["k"])
      "q" -> list.append(acc, ["q"])
      "r" -> list.append(acc, ["r"])
      "b" -> list.append(acc, ["b"])
      "n" -> list.append(acc, ["n"])
      "p" -> list.append(acc, ["p"])
      _ -> list.append(acc, [part])
    }
  })
}

fn parse_turn(turn_string: String) -> Color {
  case turn_string {
    "w" -> White
    "b" -> Black
    _ -> panic as "Invalid turn string, must be 'w' or 'b'"
  }
}

fn parse_castling(castling_string: String) -> CastlingStatus {
  case string.length(castling_string) <= 4 {
    True -> {
      let cs_parts = string.split(castling_string, "")
      list.each(cs_parts, fn(part) {
        case part {
          "K" -> Nil
          "Q" -> Nil
          "k" -> Nil
          "q" -> Nil
          "-" -> Nil
          _ -> panic as "Invalid castling string"
        }
      })

      let white_queenside_castling = string.contains(castling_string, "Q")
      let white_kingside_castling = string.contains(castling_string, "K")
      let black_queenside_castling = string.contains(castling_string, "q")
      let black_kingside_castling = string.contains(castling_string, "k")
      CastlingStatus(
        white_kingside: white_kingside_castling,
        white_queenside: white_queenside_castling,
        black_kingside: black_kingside_castling,
        black_queenside: black_queenside_castling,
      )
    }
    False -> panic as "Invalid castling string"
  }
}

fn parse_en_passant(en_passant_string: String) -> Option(Position) {
  case en_passant_string {
    "-" -> None
    _ -> {
      case string.length(en_passant_string) == 2 {
        True -> Nil
        False -> panic as "Invalid en passant string"
      }
      let en_passant_parts = string.split(en_passant_string, "")
      let assert [file_string, rank_string] = en_passant_parts
      let file = parse_file(file_string)
      let rank = parse_rank(rank_string)
      Some(Position(file: file, rank: rank))
    }
  }
}

fn parse_file(file_string: String) -> File {
  case file_string {
    "a" -> A
    "b" -> B
    "c" -> C
    "d" -> D
    "e" -> E
    "f" -> F
    "g" -> G
    "h" -> H
    _ -> panic as "Invalid file string"
  }
}

fn parse_rank(rank_string: String) -> Rank {
  case rank_string {
    "1" -> One
    "2" -> Two
    "3" -> Three
    "4" -> Four
    "5" -> Five
    "6" -> Six
    "7" -> Seven
    "8" -> Eight
    _ -> panic as "Invalid rank string"
  }
}

fn parse_halfmove(halfmove_string: String) -> HalfMove {
  string_to_int(halfmove_string)
}

fn parse_fullmove(fullmove_string: String) -> FullMove {
  string_to_int(fullmove_string)
}

fn string_to_int(string: String) -> Int {
  case string.length(string) {
    1 -> {
      case string {
        "0" -> 0
        "1" -> 1
        "2" -> 2
        "3" -> 3
        "4" -> 4
        "5" -> 5
        "6" -> 6
        "7" -> 7
        "8" -> 8
        "9" -> 9
        _ -> panic as "Invalid halfmove string"
      }
    }
    2 -> {
      let assert [tenths_place_string, ones_place_string] =
        string.split(string, "")
      let tenths_place = case tenths_place_string {
        "0" -> 0
        "1" -> 10
        "2" -> 20
        "3" -> 30
        "4" -> 40
        "5" -> 50
        "6" -> 60
        "7" -> 70
        "8" -> 80
        "9" -> 90
        _ -> panic as "Invalid halfmove string"
      }

      let ones_place = case ones_place_string {
        "0" -> 0
        "1" -> 1
        "2" -> 2
        "3" -> 3
        "4" -> 4
        "5" -> 5
        "6" -> 6
        "7" -> 7
        "8" -> 8
        "9" -> 9
        _ -> panic as "Invalid halfmove string"
      }
      tenths_place + ones_place
    }
    3 -> {
      let assert [hundreds_place_string, tenths_place_string, ones_place_string] =
        string.split(string, "")
      let hundreds_place = case hundreds_place_string {
        "0" -> 0
        "1" -> 100
        "2" -> 200
        "3" -> 300
        "4" -> 400
        "5" -> 500
        "6" -> 600
        "7" -> 700
        "8" -> 800
        "9" -> 900
        _ -> panic as "Invalid halfmove string"
      }

      let tenths_place = case tenths_place_string {
        "0" -> 0
        "1" -> 10
        "2" -> 20
        "3" -> 30
        "4" -> 40
        "5" -> 50
        "6" -> 60
        "7" -> 70
        "8" -> 80
        "9" -> 90
        _ -> panic as "Invalid halfmove string"
      }

      let ones_place = case ones_place_string {
        "0" -> 0
        "1" -> 1
        "2" -> 2
        "3" -> 3
        "4" -> 4
        "5" -> 5
        "6" -> 6
        "7" -> 7
        "8" -> 8
        "9" -> 9
        _ -> panic as "Invalid halfmove string"
      }
      hundreds_place + tenths_place + ones_place
    }
    _ -> panic as "Invalid halfmove string"
  }
}

// SAN

pub fn move_san_from_string(san: String) -> Result(MoveSan, ErrorSan) {
  case string.to_graphemes(san) {
    [] -> panic as "Cannot parse empty string."
    [piece_letter, ..rest]
      if piece_letter == "K"
      || piece_letter == "Q"
      || piece_letter == "R"
      || piece_letter == "B"
      || piece_letter == "N"
    -> {
      let check_or_checkmate = case list.last(rest) {
        Ok("+") -> Some(Check)
        Ok("#") -> Some(CheckMate)
        _ -> None
      }

      let moving_piece = case piece_letter {
        "K" -> King
        "Q" -> Queen
        "R" -> Rook
        "B" -> Bishop
        "N" -> Knight
        _ -> panic as "Invalid piece letter."
      }

      let promotion = None

      let capture = list.contains(rest, "x")

      let positional_information =
        list.filter(rest, fn(grapheme) {
          grapheme != "+" && grapheme != "#" && grapheme != "x"
        })

      case list.length(positional_information) {
        4 -> {
          let assert [from_file, from_rank, to_file, to_rank] =
            positional_information
          let from_file = case from_file {
            "a" -> A
            "b" -> B
            "c" -> C
            "d" -> D
            "e" -> E
            "f" -> F
            "g" -> G
            "h" -> H
            _ -> panic as "Invalid file."
          }

          let from_rank = case from_rank {
            "1" -> One
            "2" -> Two
            "3" -> Three
            "4" -> Four
            "5" -> Five
            "6" -> Six
            "7" -> Seven
            "8" -> Eight
            _ -> panic as "Invalid rank."
          }

          let to_file = case to_file {
            "a" -> A
            "b" -> B
            "c" -> C
            "d" -> D
            "e" -> E
            "f" -> F
            "g" -> G
            "h" -> H
            _ -> panic as "Invalid file."
          }

          let to_rank = case to_rank {
            "1" -> One
            "2" -> Two
            "3" -> Three
            "4" -> Four
            "5" -> Five
            "6" -> Six
            "7" -> Seven
            "8" -> Eight
            _ -> panic as "Invalid rank."
          }

          Ok(MoveSanNormal(
            from: Some(PositionSan(file: Some(from_file), rank: Some(from_rank))),
            to: Position(file: to_file, rank: to_rank),
            moving_piece: moving_piece,
            capture: capture,
            promotion: promotion,
            maybe_check_or_checkmate: check_or_checkmate,
          ))
        }
        3 -> {
          let assert [maybe_from_file_or_from_rank, to_file, to_rank] =
            positional_information
          case maybe_from_file_or_from_rank {
            "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" -> {
              let from_file = case maybe_from_file_or_from_rank {
                "a" -> A
                "b" -> B
                "c" -> C
                "d" -> D
                "e" -> E
                "f" -> F
                "g" -> G
                "h" -> H
                _ -> panic as "Invalid file"
              }

              let to_file = case to_file {
                "a" -> A
                "b" -> B
                "c" -> C
                "d" -> D
                "e" -> E
                "f" -> F
                "g" -> G
                "h" -> H
                _ -> panic as "Invalid file."
              }

              let to_rank = case to_rank {
                "1" -> One
                "2" -> Two
                "3" -> Three
                "4" -> Four
                "5" -> Five
                "6" -> Six
                "7" -> Seven
                "8" -> Eight
                _ -> panic as "Invalid rank."
              }

              Ok(MoveSanNormal(
                from: Some(PositionSan(file: Some(from_file), rank: None)),
                to: Position(file: to_file, rank: to_rank),
                moving_piece: moving_piece,
                capture: capture,
                promotion: promotion,
                maybe_check_or_checkmate: check_or_checkmate,
              ))
            }
            "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" -> {
              let from_rank = case maybe_from_file_or_from_rank {
                "1" -> One
                "2" -> Two
                "3" -> Three
                "4" -> Four
                "5" -> Five
                "6" -> Six
                "7" -> Seven
                "8" -> Eight
                _ -> panic as "Invalid rank."
              }

              let to_file = case to_file {
                "a" -> A
                "b" -> B
                "c" -> C
                "d" -> D
                "e" -> E
                "f" -> F
                "g" -> G
                "h" -> H
                _ -> panic as "Invalid file."
              }

              let to_rank = case to_rank {
                "1" -> One
                "2" -> Two
                "3" -> Three
                "4" -> Four
                "5" -> Five
                "6" -> Six
                "7" -> Seven
                "8" -> Eight
                _ -> panic as "Invalid rank."
              }

              Ok(MoveSanNormal(
                from: Some(PositionSan(file: None, rank: Some(from_rank))),
                to: Position(file: to_file, rank: to_rank),
                moving_piece: moving_piece,
                capture: capture,
                promotion: promotion,
                maybe_check_or_checkmate: check_or_checkmate,
              ))
            }
            _ -> Error(InvalidPositionalInformation)
          }
        }
        2 -> {
          let assert [to_file, to_rank] = positional_information
          let to_file = case to_file {
            "a" -> A
            "b" -> B
            "c" -> C
            "d" -> D
            "e" -> E
            "f" -> F
            "g" -> G
            "h" -> H
            _ -> panic as "Invalid file."
          }

          let to_rank = case to_rank {
            "1" -> One
            "2" -> Two
            "3" -> Three
            "4" -> Four
            "5" -> Five
            "6" -> Six
            "7" -> Seven
            "8" -> Eight
            _ -> panic as "Invalid rank."
          }

          Ok(MoveSanNormal(
            from: None,
            to: Position(file: to_file, rank: to_rank),
            moving_piece: moving_piece,
            capture: capture,
            promotion: promotion,
            maybe_check_or_checkmate: check_or_checkmate,
          ))
        }
        _ -> {
          Error(InvalidPositionalInformation)
        }
      }
    }
    ["O", ..rest] | ["0", ..rest] -> {
      case rest {
        ["-", "O", "-", "O", ..checks_or_checkmates]
        | ["-", "0", "-", "0", ..checks_or_checkmates] -> {
          case checks_or_checkmates {
            [] -> {
              Ok(MoveSanCastle(side: QueenSide, maybe_check_or_checkmate: None))
            }
            ["+", ..] -> {
              Ok(MoveSanCastle(
                side: QueenSide,
                maybe_check_or_checkmate: Some(Check),
              ))
            }
            ["#", ..] -> {
              Ok(MoveSanCastle(
                side: QueenSide,
                maybe_check_or_checkmate: Some(CheckMate),
              ))
            }
            _ -> Error(InvalidCastleString)
          }
        }
        ["-", "O", ..checks_or_checkmates] | ["-", "0", ..checks_or_checkmates] -> {
          case checks_or_checkmates {
            [] -> {
              Ok(MoveSanCastle(side: KingSide, maybe_check_or_checkmate: None))
            }
            ["+", ..] -> {
              Ok(MoveSanCastle(
                side: KingSide,
                maybe_check_or_checkmate: Some(Check),
              ))
            }
            ["#", ..] -> {
              Ok(MoveSanCastle(
                side: KingSide,
                maybe_check_or_checkmate: Some(CheckMate),
              ))
            }
            _ -> Error(InvalidCastleString)
          }
        }
        _ -> Error(InvalidCastleString)
      }
    }
    [pawn_move_first_grapheme, ..rest]
      if pawn_move_first_grapheme == "a"
      || pawn_move_first_grapheme == "b"
      || pawn_move_first_grapheme == "c"
      || pawn_move_first_grapheme == "d"
      || pawn_move_first_grapheme == "e"
      || pawn_move_first_grapheme == "f"
      || pawn_move_first_grapheme == "g"
      || pawn_move_first_grapheme == "h"
    -> {
      let is_en_passant =
        string.contains(
          string_tree.to_string(string_tree.from_strings(rest)),
          "e.p.",
        )

      let rest =
        string.replace(
          string_tree.to_string(string_tree.from_strings(rest)),
          "e.p.",
          "",
        )

      let rest = string.trim(rest)

      let rest = string.to_graphemes(rest)

      let capture = list.contains(rest, "x")

      let rest = list.filter(rest, fn(grapheme) { grapheme != "x" })

      let #(promotion_segment, rest) =
        list.partition([pawn_move_first_grapheme, ..rest], fn(grapheme) {
          case grapheme {
            "=" | "Q" | "R" | "B" | "N" -> True
            _ -> False
          }
        })

      let promotion = case promotion_segment {
        [] -> None
        ["=", "Q", ..] -> Some(Queen)
        ["=", "R", ..] -> Some(Rook)
        ["=", "B", ..] -> Some(Bishop)
        ["=", "N", ..] -> Some(Knight)
        _ -> panic as "Invalid promotion segment."
      }

      let maybe_check_or_checkmate = case list.last(rest) {
        Ok("+") -> Some(Check)
        Ok("#") -> Some(CheckMate)
        _ -> None
      }

      let positional_information =
        list.filter(rest, fn(grapheme) { grapheme != "+" && grapheme != "#" })

      case list.length(positional_information) {
        4 -> {
          let assert [from_file, from_rank, to_file, to_rank] =
            positional_information
          let from_file = case from_file {
            "a" -> A
            "b" -> B
            "c" -> C
            "d" -> D
            "e" -> E
            "f" -> F
            "g" -> G
            "h" -> H
            _ -> panic as "Invalid file."
          }

          let from_rank = case from_rank {
            "1" -> One
            "2" -> Two
            "3" -> Three
            "4" -> Four
            "5" -> Five
            "6" -> Six
            "7" -> Seven
            "8" -> Eight
            _ -> panic as "Invalid rank."
          }

          let to_file = case to_file {
            "a" -> A
            "b" -> B
            "c" -> C
            "d" -> D
            "e" -> E
            "f" -> F
            "g" -> G
            "h" -> H
            _ -> panic as "Invalid file."
          }

          let to_rank = case to_rank {
            "1" -> One
            "2" -> Two
            "3" -> Three
            "4" -> Four
            "5" -> Five
            "6" -> Six
            "7" -> Seven
            "8" -> Eight
            _ -> panic as "Invalid rank."
          }

          case is_en_passant {
            True -> {
              Ok(MoveSanEnPassant(
                from: Some(PositionSan(
                  file: Some(from_file),
                  rank: Some(from_rank),
                )),
                to: Position(file: to_file, rank: to_rank),
                maybe_check_or_checkmate: maybe_check_or_checkmate,
              ))
            }
            False -> {
              Ok(MoveSanNormal(
                from: Some(PositionSan(
                  file: Some(from_file),
                  rank: Some(from_rank),
                )),
                to: Position(file: to_file, rank: to_rank),
                moving_piece: Pawn,
                capture: capture,
                promotion: promotion,
                maybe_check_or_checkmate: maybe_check_or_checkmate,
              ))
            }
          }
        }
        3 -> {
          let assert [maybe_from_file_or_from_rank, to_file, to_rank] =
            positional_information
          case maybe_from_file_or_from_rank {
            "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" -> {
              let from_file = case maybe_from_file_or_from_rank {
                "a" -> A
                "b" -> B
                "c" -> C
                "d" -> D
                "e" -> E
                "f" -> F
                "g" -> G
                "h" -> H
                _ -> panic as "Invalid file"
              }

              let to_file = case to_file {
                "a" -> A
                "b" -> B
                "c" -> C
                "d" -> D
                "e" -> E
                "f" -> F
                "g" -> G
                "h" -> H
                _ -> panic as "Invalid file."
              }

              let to_rank = case to_rank {
                "1" -> One
                "2" -> Two
                "3" -> Three
                "4" -> Four
                "5" -> Five
                "6" -> Six
                "7" -> Seven
                "8" -> Eight
                _ -> panic as "Invalid rank."
              }

              case is_en_passant {
                True -> {
                  Ok(MoveSanEnPassant(
                    from: Some(PositionSan(file: Some(from_file), rank: None)),
                    to: Position(file: to_file, rank: to_rank),
                    maybe_check_or_checkmate: maybe_check_or_checkmate,
                  ))
                }
                False -> {
                  Ok(MoveSanNormal(
                    from: Some(PositionSan(file: Some(from_file), rank: None)),
                    to: Position(file: to_file, rank: to_rank),
                    moving_piece: Pawn,
                    capture: capture,
                    promotion: promotion,
                    maybe_check_or_checkmate: maybe_check_or_checkmate,
                  ))
                }
              }
            }
            "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" -> {
              let from_rank = case maybe_from_file_or_from_rank {
                "1" -> One
                "2" -> Two
                "3" -> Three
                "4" -> Four
                "5" -> Five
                "6" -> Six
                "7" -> Seven
                "8" -> Eight
                _ -> panic as "Invalid rank."
              }

              let to_file = case to_file {
                "a" -> A
                "b" -> B
                "c" -> C
                "d" -> D
                "e" -> E
                "f" -> F
                "g" -> G
                "h" -> H
                _ -> panic as "Invalid file."
              }

              let to_rank = case to_rank {
                "1" -> One
                "2" -> Two
                "3" -> Three
                "4" -> Four
                "5" -> Five
                "6" -> Six
                "7" -> Seven
                "8" -> Eight
                _ -> panic as "Invalid rank."
              }

              case is_en_passant {
                True -> {
                  Ok(MoveSanEnPassant(
                    from: Some(PositionSan(file: None, rank: Some(from_rank))),
                    to: Position(file: to_file, rank: to_rank),
                    maybe_check_or_checkmate: maybe_check_or_checkmate,
                  ))
                }
                False -> {
                  Ok(MoveSanNormal(
                    from: Some(PositionSan(file: None, rank: Some(from_rank))),
                    to: Position(file: to_file, rank: to_rank),
                    moving_piece: Pawn,
                    capture: capture,
                    promotion: promotion,
                    maybe_check_or_checkmate: maybe_check_or_checkmate,
                  ))
                }
              }
            }
            _ -> Error(InvalidPositionalInformation)
          }
        }
        2 -> {
          let assert [to_file, to_rank] = positional_information
          let to_file = case to_file {
            "a" -> A
            "b" -> B
            "c" -> C
            "d" -> D
            "e" -> E
            "f" -> F
            "g" -> G
            "h" -> H
            _ -> panic as "Invalid file."
          }

          let to_rank = case to_rank {
            "1" -> One
            "2" -> Two
            "3" -> Three
            "4" -> Four
            "5" -> Five
            "6" -> Six
            "7" -> Seven
            "8" -> Eight
            _ -> panic as "Invalid rank."
          }

          case is_en_passant {
            True -> {
              Ok(MoveSanEnPassant(
                from: None,
                to: Position(file: to_file, rank: to_rank),
                maybe_check_or_checkmate: maybe_check_or_checkmate,
              ))
            }
            False -> {
              Ok(MoveSanNormal(
                from: None,
                to: Position(file: to_file, rank: to_rank),
                moving_piece: Pawn,
                capture: capture,
                promotion: promotion,
                maybe_check_or_checkmate: maybe_check_or_checkmate,
              ))
            }
          }
        }
        _ -> Error(InvalidPositionalInformation)
      }
    }
    _ -> Error(InvalidMoveString)
  }
}
