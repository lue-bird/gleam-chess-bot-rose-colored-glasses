import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/set
import gleam/string

pub type Bitboard =
  Int

pub fn bitboard_from_int(bitboard: Int) -> Bitboard {
  bitboard
}

pub fn bitboard_empty() -> Bitboard {
  0
}

pub fn full_bitboard() -> Bitboard {
  0xffffffffffffffff
}

fn position_to_bitboard(position: Position) -> Bitboard {
  int.bitwise_shift_left(0x0000000000000001, position |> position_to_int)
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
      let first_digit_bitboard =
        bitboard_and(bitboard, bitboard_from_int(lsb_digit))
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
      let first_digit_bitboard =
        bitboard_and(bitboard, bitboard_from_int(msb_digit))
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
    black_pawn_bitboard: Bitboard,
    white_king_bitboard: Bitboard,
    white_queen_bitboard: Bitboard,
    white_rook_bitboard: Bitboard,
    white_bishop_bitboard: Bitboard,
    white_knight_bitboard: Bitboard,
    white_pawn_bitboard: Bitboard,
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

pub fn position_to_string(position: Position) -> String {
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
  case i >= 0 && i < 64 {
    True -> {
      let assert Some(file) = int_to_file(i % 8)
      let assert Some(rank) = int_to_rank(i / 8)
      Ok(Position(file, rank))
    }
    False -> Error("Invalid position")
  }
}

pub fn int_to_rank(i: Int) -> Option(Rank) {
  case i {
    0 -> Some(One)
    1 -> Some(Two)
    2 -> Some(Three)
    3 -> Some(Four)
    4 -> Some(Five)
    5 -> Some(Six)
    6 -> Some(Seven)
    7 -> Some(Eight)
    _ -> None
  }
}

pub fn int_to_file(i: Int) -> Option(File) {
  case i {
    0 -> Some(A)
    1 -> Some(B)
    2 -> Some(C)
    3 -> Some(D)
    4 -> Some(E)
    5 -> Some(F)
    6 -> Some(G)
    7 -> Some(H)
    _ -> None
  }
}

// a function that returns a position that is x squares rank-wise away from the given position and y squares file-wise away from the given position
pub fn position_offset_by(
  position: Position,
  x x: Int,
  y y: Int,
) -> Option(Position) {
  use file <- option.then(int_to_file(file_to_int(position.file) + x))
  use rank <- option.then(int_to_rank(rank_to_int(position.rank) + y))
  Some(Position(file, rank))
}

fn position_offset_y_positive_if_white_negative_if_black(
  offset: Int,
  color: Color,
) {
  case color {
    White -> offset
    Black -> -offset
  }
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

pub fn board_remove_piece_at(board: BoardBB, position: Position) -> BoardBB {
  let bitboard = bitboard_not(from_position(position))

  let new_board = case board_piece_at(board, position) {
    None -> board
    Some(Piece(color: color, kind: kind)) if color == White && kind == King -> {
      BoardBB(
        ..board,
        white_king_bitboard: bitboard_and(bitboard, board.white_king_bitboard),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == White && kind == Queen -> {
      BoardBB(
        ..board,
        white_queen_bitboard: bitboard_and(bitboard, board.white_queen_bitboard),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == White && kind == Rook -> {
      BoardBB(
        ..board,
        white_rook_bitboard: bitboard_and(bitboard, board.white_rook_bitboard),
      )
    }

    Some(Piece(color: color, kind: kind)) if color == White && kind == Bishop -> {
      BoardBB(
        ..board,
        white_bishop_bitboard: bitboard_and(
          bitboard,
          board.white_bishop_bitboard,
        ),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == White && kind == Knight -> {
      BoardBB(
        ..board,
        white_knight_bitboard: bitboard_and(
          bitboard,
          board.white_knight_bitboard,
        ),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == White && kind == Pawn -> {
      BoardBB(
        ..board,
        white_pawn_bitboard: bitboard_and(bitboard, board.white_pawn_bitboard),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == Black && kind == King -> {
      BoardBB(
        ..board,
        black_king_bitboard: bitboard_and(bitboard, board.black_king_bitboard),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == Black && kind == Queen -> {
      BoardBB(
        ..board,
        black_queen_bitboard: bitboard_and(bitboard, board.black_queen_bitboard),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == Black && kind == Rook -> {
      BoardBB(
        ..board,
        black_rook_bitboard: bitboard_and(bitboard, board.black_rook_bitboard),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == Black && kind == Bishop -> {
      BoardBB(
        ..board,
        black_bishop_bitboard: bitboard_and(
          bitboard,
          board.black_bishop_bitboard,
        ),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == Black && kind == Knight -> {
      BoardBB(
        ..board,
        black_knight_bitboard: bitboard_and(
          bitboard,
          board.black_knight_bitboard,
        ),
      )
    }
    Some(Piece(color: color, kind: kind)) if color == Black && kind == Pawn -> {
      BoardBB(
        ..board,
        black_pawn_bitboard: bitboard_and(bitboard, board.black_pawn_bitboard),
      )
    }
    _ -> {
      // TODO why is this needed?
      panic as "Invalid piece"
    }
  }
  new_board
}

pub fn board_set_piece_at(
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
        white_pawn_bitboard: bitboard_or(bitboard, board.white_pawn_bitboard),
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
        black_pawn_bitboard: bitboard_or(bitboard, board.black_pawn_bitboard),
      )
    }
    _ -> {
      panic as "Invalid piece"
    }
  }
  new_board
}

pub fn board_piece_at(board: BoardBB, position: Position) {
  let bitboard = from_position(position)
  let black_king_bb_compare = bitboard_and(board.black_king_bitboard, bitboard)
  let black_queen_bb_compare =
    bitboard_and(board.black_queen_bitboard, bitboard)
  let black_rook_bb_compare = bitboard_and(board.black_rook_bitboard, bitboard)
  let black_bishop_bb_compare =
    bitboard_and(board.black_bishop_bitboard, bitboard)
  let black_knight_bb_compare =
    bitboard_and(board.black_knight_bitboard, bitboard)
  let black_pawns_bb_compare = bitboard_and(board.black_pawn_bitboard, bitboard)
  let white_king_bb_compare = bitboard_and(board.white_king_bitboard, bitboard)
  let white_queen_bb_compare =
    bitboard_and(board.white_queen_bitboard, bitboard)
  let white_rook_bb_compare = bitboard_and(board.white_rook_bitboard, bitboard)
  let white_bishop_bb_compare =
    bitboard_and(board.white_bishop_bitboard, bitboard)
  let white_knight_bb_compare =
    bitboard_and(board.white_knight_bitboard, bitboard)
  let white_pawns_bb_compare = bitboard_and(board.white_pawn_bitboard, bitboard)

  let piece = case bitboard {
    0 -> None
    _ -> {
      let piece = case Nil {
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
    board.black_pawn_bitboard,
    board.white_king_bitboard,
    board.white_queen_bitboard,
    board.white_rook_bitboard,
    board.white_bishop_bitboard,
    board.white_knight_bitboard,
    board.white_pawn_bitboard,
  ]

  let positions =
    list.fold(list_of_bitboards, set.new(), fn(acc, bitboard) {
      let positions = bitboard_occupied_positions(bitboard)
      let positions = set.from_list(positions)
      set.union(acc, positions)
    })

  Ok(set.to_list(positions))
}

pub fn bitboard_occupied_positions(bitboard: Bitboard) -> List(Position) {
  case bitboard {
    0 -> []
    _ -> {
      case bitboard_and(bitboard, bitboard_from_int(0x8000000000000000)) {
        0 ->
          bitboard_occupied_positions_inner(
            bitboard_shift_left(bitboard, 1),
            62,
          )
        _ -> {
          [
            Position(H, Eight),
            ..bitboard_occupied_positions_inner(
              bitboard_shift_left(bitboard, 1),
              62,
            )
          ]
        }
      }
    }
  }
}

pub fn bitboard_occupied_positions_inner(
  bitboard: Bitboard,
  count: Int,
) -> List(Position) {
  case count < 0 {
    True -> []
    False -> {
      case bitboard_and(bitboard, bitboard_from_int(0x8000000000000000)) {
        0 ->
          bitboard_occupied_positions_inner(
            bitboard_shift_left(bitboard, 1),
            count - 1,
          )
        _ -> {
          let assert Ok(position_dest) = position_from_int(count)
          [
            position_dest,
            ..bitboard_occupied_positions_inner(
              bitboard_shift_left(bitboard, 1),
              count - 1,
            )
          ]
        }
      }
    }
  }
}

pub fn from_position(position: Position) -> Bitboard {
  let bitboard =
    bitboard_shift_left(bitboard_from_int(1), position_to_int(position))
  bitboard
}

/// https://www.net-chess.com/sanfaq.html
pub type MoveSan {
  MoveSanNormal(
    from: Position,
    to: Position,
    moving_piece: PieceKind,
    capture: Bool,
    promotion: Option(PieceKind),
  )
  MoveSanCastle(side: CastleSide)
  MoveSanEnPassant(from: Position, to: Position)
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

pub fn move_san_to_string(move_san: MoveSan) {
  // https://www.chessprogramming.org/Algebraic_Chess_Notation#Standard_Algebraic_Notation_.28SAN.29
  case move_san {
    MoveSanCastle(side) ->
      case side {
        KingSide -> "O-O"
        QueenSide -> "O-O-O"
      }
    MoveSanEnPassant(from, to) ->
      position_to_string(from) <> "x" <> position_to_string(to) <> "e.p."
    MoveSanNormal(from, to, moving_piece, capture, promotion) -> {
      case promotion {
        None -> {
          let moving_piece_kind_as_letter_string = case moving_piece {
            Pawn -> ""
            Knight -> "N"
            Bishop -> "B"
            Rook -> "R"
            Queen -> "Q"
            King -> "K"
          }
          moving_piece_kind_as_letter_string
          <> position_to_string(from)
          <> {
            case capture {
              True -> "x"
              False -> ""
            }
          }
          <> position_to_string(to)
        }
        Some(promotion_piece_kind) -> {
          let promotion_piece_kind_as_letter_string = case
            promotion_piece_kind
          {
            Pawn ->
              // impossible case
              ""
            Knight -> "N"
            Bishop -> "B"
            Rook -> "R"
            Queen -> "Q"
            King -> "K"
          }
          position_to_string(from)
          <> {
            case capture {
              True -> "x"
              False -> ""
            }
          }
          <> position_to_string(to)
          <> "="
          <> promotion_piece_kind_as_letter_string
        }
      }
    }
  }
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

pub fn fen_from_string(fen: String) -> Result(Fen, String) {
  case string.split(string.trim(fen), " ") {
    [
      board_string,
      turn_string,
      castling_string,
      en_passant_string,
      ..optional_halfmove_fullmove
    ] -> {
      case
        parse_board(board_string),
        parse_en_passant(en_passant_string),
        parse_turn(turn_string),
        parse_castling(castling_string),
        case optional_halfmove_fullmove {
          [halfmove_string, _] -> parse_halfmove(halfmove_string)
          _ -> Ok(0)
        },
        case optional_halfmove_fullmove {
          [_, fullmove_string] -> parse_fullmove(fullmove_string)
          _ -> Ok(1)
        }
      {
        Ok(parsed_board),
          Ok(parsed_en_passant),
          Ok(parsed_turn),
          Ok(parsed_castling),
          Ok(parsed_halfmove),
          Ok(parsed_fullmove)
        ->
          Ok(Fen(
            board: parsed_board,
            turn: parsed_turn,
            castling: parsed_castling,
            en_passant: parsed_en_passant,
            halfmove: parsed_halfmove,
            fullmove: parsed_fullmove,
          ))
        board_result,
          en_passant_result,
          turn_result,
          castling_result,
          halfmove_result,
          fullmove_result
        ->
          Error(
            [
              board_result |> result_to_error,
              en_passant_result |> result_to_error,
              turn_result |> result_to_error,
              castling_result |> result_to_error,
              halfmove_result |> result_to_error,
              fullmove_result |> result_to_error,
            ]
            |> list_somes
            |> string.join(" and "),
          )
      }
    }
    _ -> Error("Invalid FEN string, weird amount of segments")
  }
}

fn result_to_error(result: Result(_success, error)) -> Option(error) {
  case result {
    Ok(_) -> None
    Error(error) -> Some(error)
  }
}

fn list_fold_indexed_while_ok(
  list: List(element),
  initial_folded: folded,
  reduce: fn(folded, element, Int) -> Result(folded, error),
) -> Result(folded, error) {
  // could be optimized
  list.index_fold(list, Ok(initial_folded), fn(so_far, element, index) {
    case so_far {
      Error(error) -> Error(error)
      Ok(so_far_success) -> reduce(so_far_success, element, index)
    }
  })
}

// This function parses the board part of the FEN string
pub fn parse_board(board_string: String) -> Result(BoardBB, String) {
  // in the context of this function, rank means a an entire row of the board 
  // represented as a string of piece chars and numbers for empty spaces
  // example: "rnbqk1nr"

  let list_of_ranks_as_strings = string.split(board_string, "/")
  list_fold_indexed_while_ok(
    list_of_ranks_as_strings,
    BoardBB(
      black_king_bitboard: 0,
      black_queen_bitboard: 0,
      black_rook_bitboard: 0,
      black_bishop_bitboard: 0,
      black_knight_bitboard: 0,
      black_pawn_bitboard: 0,
      white_king_bitboard: 0,
      white_queen_bitboard: 0,
      white_rook_bitboard: 0,
      white_bishop_bitboard: 0,
      white_knight_bitboard: 0,
      white_pawn_bitboard: 0,
    ),
    fn(acc, rank_as_string, rank_index) {
      use rank_index <- result.then(
        int_to_rank(7 - rank_index)
        |> option.to_result("bug! invalid rank index"),
      )
      let rank_parts = string.to_graphemes(rank_as_string)
      let expanded_rank = expand_rank(rank_parts)
      list_fold_indexed_while_ok(
        expanded_rank,
        acc,
        fn(acc, square, file_index) {
          use file_index <- result.then(
            int_to_file(file_index)
            |> option.to_result("bug! invalid file index"),
          )
          case square {
            "" -> Ok(acc)
            "K" -> {
              let new_white_king_bitboard =
                from_position(Position(file: file_index, rank: rank_index))
              Ok(BoardBB(
                black_king_bitboard: acc.black_king_bitboard,
                black_queen_bitboard: acc.black_queen_bitboard,
                black_rook_bitboard: acc.black_rook_bitboard,
                black_bishop_bitboard: acc.black_bishop_bitboard,
                black_knight_bitboard: acc.black_knight_bitboard,
                black_pawn_bitboard: acc.black_pawn_bitboard,
                white_king_bitboard: bitboard_or(
                  acc.white_king_bitboard,
                  new_white_king_bitboard,
                ),
                white_queen_bitboard: acc.white_queen_bitboard,
                white_rook_bitboard: acc.white_rook_bitboard,
                white_bishop_bitboard: acc.white_bishop_bitboard,
                white_knight_bitboard: acc.white_knight_bitboard,
                white_pawn_bitboard: acc.white_pawn_bitboard,
              ))
            }
            "Q" -> {
              let new_white_queen_bitboard =
                from_position(Position(file: file_index, rank: rank_index))
              Ok(BoardBB(
                black_king_bitboard: acc.black_king_bitboard,
                black_queen_bitboard: acc.black_queen_bitboard,
                black_rook_bitboard: acc.black_rook_bitboard,
                black_bishop_bitboard: acc.black_bishop_bitboard,
                black_knight_bitboard: acc.black_knight_bitboard,
                black_pawn_bitboard: acc.black_pawn_bitboard,
                white_king_bitboard: acc.white_king_bitboard,
                white_queen_bitboard: bitboard_or(
                  acc.white_queen_bitboard,
                  new_white_queen_bitboard,
                ),
                white_rook_bitboard: acc.white_rook_bitboard,
                white_bishop_bitboard: acc.white_bishop_bitboard,
                white_knight_bitboard: acc.white_knight_bitboard,
                white_pawn_bitboard: acc.white_pawn_bitboard,
              ))
            }
            "R" -> {
              let new_white_rook_bitboard =
                from_position(Position(file: file_index, rank: rank_index))
              Ok(BoardBB(
                black_king_bitboard: acc.black_king_bitboard,
                black_queen_bitboard: acc.black_queen_bitboard,
                black_rook_bitboard: acc.black_rook_bitboard,
                black_bishop_bitboard: acc.black_bishop_bitboard,
                black_knight_bitboard: acc.black_knight_bitboard,
                black_pawn_bitboard: acc.black_pawn_bitboard,
                white_king_bitboard: acc.white_king_bitboard,
                white_queen_bitboard: acc.white_queen_bitboard,
                white_rook_bitboard: bitboard_or(
                  acc.white_rook_bitboard,
                  new_white_rook_bitboard,
                ),
                white_bishop_bitboard: acc.white_bishop_bitboard,
                white_knight_bitboard: acc.white_knight_bitboard,
                white_pawn_bitboard: acc.white_pawn_bitboard,
              ))
            }
            "B" -> {
              let new_white_bishop_bitboard =
                from_position(Position(file: file_index, rank: rank_index))
              Ok(BoardBB(
                black_king_bitboard: acc.black_king_bitboard,
                black_queen_bitboard: acc.black_queen_bitboard,
                black_rook_bitboard: acc.black_rook_bitboard,
                black_bishop_bitboard: acc.black_bishop_bitboard,
                black_knight_bitboard: acc.black_knight_bitboard,
                black_pawn_bitboard: acc.black_pawn_bitboard,
                white_king_bitboard: acc.white_king_bitboard,
                white_queen_bitboard: acc.white_queen_bitboard,
                white_rook_bitboard: acc.white_rook_bitboard,
                white_bishop_bitboard: bitboard_or(
                  acc.white_bishop_bitboard,
                  new_white_bishop_bitboard,
                ),
                white_knight_bitboard: acc.white_knight_bitboard,
                white_pawn_bitboard: acc.white_pawn_bitboard,
              ))
            }
            "N" -> {
              let new_white_knight_bitboard =
                from_position(Position(file: file_index, rank: rank_index))
              Ok(BoardBB(
                black_king_bitboard: acc.black_king_bitboard,
                black_queen_bitboard: acc.black_queen_bitboard,
                black_rook_bitboard: acc.black_rook_bitboard,
                black_bishop_bitboard: acc.black_bishop_bitboard,
                black_knight_bitboard: acc.black_knight_bitboard,
                black_pawn_bitboard: acc.black_pawn_bitboard,
                white_king_bitboard: acc.white_king_bitboard,
                white_queen_bitboard: acc.white_queen_bitboard,
                white_rook_bitboard: acc.white_rook_bitboard,
                white_bishop_bitboard: acc.white_bishop_bitboard,
                white_knight_bitboard: bitboard_or(
                  acc.white_knight_bitboard,
                  new_white_knight_bitboard,
                ),
                white_pawn_bitboard: acc.white_pawn_bitboard,
              ))
            }
            "P" -> {
              let new_white_pawn_bitboard =
                from_position(Position(file: file_index, rank: rank_index))
              Ok(BoardBB(
                black_king_bitboard: acc.black_king_bitboard,
                black_queen_bitboard: acc.black_queen_bitboard,
                black_rook_bitboard: acc.black_rook_bitboard,
                black_bishop_bitboard: acc.black_bishop_bitboard,
                black_knight_bitboard: acc.black_knight_bitboard,
                black_pawn_bitboard: acc.black_pawn_bitboard,
                white_king_bitboard: acc.white_king_bitboard,
                white_queen_bitboard: acc.white_queen_bitboard,
                white_rook_bitboard: acc.white_rook_bitboard,
                white_bishop_bitboard: acc.white_bishop_bitboard,
                white_knight_bitboard: acc.white_knight_bitboard,
                white_pawn_bitboard: bitboard_or(
                  acc.white_pawn_bitboard,
                  new_white_pawn_bitboard,
                ),
              ))
            }
            "k" -> {
              let new_black_king_bitboard =
                from_position(Position(file: file_index, rank: rank_index))
              Ok(BoardBB(
                black_king_bitboard: bitboard_or(
                  acc.black_king_bitboard,
                  new_black_king_bitboard,
                ),
                black_queen_bitboard: acc.black_queen_bitboard,
                black_rook_bitboard: acc.black_rook_bitboard,
                black_bishop_bitboard: acc.black_bishop_bitboard,
                black_knight_bitboard: acc.black_knight_bitboard,
                black_pawn_bitboard: acc.black_pawn_bitboard,
                white_king_bitboard: acc.white_king_bitboard,
                white_queen_bitboard: acc.white_queen_bitboard,
                white_rook_bitboard: acc.white_rook_bitboard,
                white_bishop_bitboard: acc.white_bishop_bitboard,
                white_knight_bitboard: acc.white_knight_bitboard,
                white_pawn_bitboard: acc.white_pawn_bitboard,
              ))
            }
            "q" -> {
              let new_black_queen_bitboard =
                from_position(Position(file: file_index, rank: rank_index))
              Ok(BoardBB(
                black_king_bitboard: acc.black_king_bitboard,
                black_queen_bitboard: bitboard_or(
                  acc.black_queen_bitboard,
                  new_black_queen_bitboard,
                ),
                black_rook_bitboard: acc.black_rook_bitboard,
                black_bishop_bitboard: acc.black_bishop_bitboard,
                black_knight_bitboard: acc.black_knight_bitboard,
                black_pawn_bitboard: acc.black_pawn_bitboard,
                white_king_bitboard: acc.white_king_bitboard,
                white_queen_bitboard: acc.white_queen_bitboard,
                white_rook_bitboard: acc.white_rook_bitboard,
                white_bishop_bitboard: acc.white_bishop_bitboard,
                white_knight_bitboard: acc.white_knight_bitboard,
                white_pawn_bitboard: acc.white_pawn_bitboard,
              ))
            }
            "r" -> {
              let new_black_rook_bitboard =
                from_position(Position(file: file_index, rank: rank_index))
              Ok(BoardBB(
                black_king_bitboard: acc.black_king_bitboard,
                black_queen_bitboard: acc.black_queen_bitboard,
                black_rook_bitboard: bitboard_or(
                  acc.black_rook_bitboard,
                  new_black_rook_bitboard,
                ),
                black_bishop_bitboard: acc.black_bishop_bitboard,
                black_knight_bitboard: acc.black_knight_bitboard,
                black_pawn_bitboard: acc.black_pawn_bitboard,
                white_king_bitboard: acc.white_king_bitboard,
                white_queen_bitboard: acc.white_queen_bitboard,
                white_rook_bitboard: acc.white_rook_bitboard,
                white_bishop_bitboard: acc.white_bishop_bitboard,
                white_knight_bitboard: acc.white_knight_bitboard,
                white_pawn_bitboard: acc.white_pawn_bitboard,
              ))
            }
            "b" -> {
              let new_black_bishop_bitboard =
                from_position(Position(file: file_index, rank: rank_index))
              Ok(BoardBB(
                black_king_bitboard: acc.black_king_bitboard,
                black_queen_bitboard: acc.black_queen_bitboard,
                black_rook_bitboard: acc.black_rook_bitboard,
                black_bishop_bitboard: bitboard_or(
                  acc.black_bishop_bitboard,
                  new_black_bishop_bitboard,
                ),
                black_knight_bitboard: acc.black_knight_bitboard,
                black_pawn_bitboard: acc.black_pawn_bitboard,
                white_king_bitboard: acc.white_king_bitboard,
                white_queen_bitboard: acc.white_queen_bitboard,
                white_rook_bitboard: acc.white_rook_bitboard,
                white_bishop_bitboard: acc.white_bishop_bitboard,
                white_knight_bitboard: acc.white_knight_bitboard,
                white_pawn_bitboard: acc.white_pawn_bitboard,
              ))
            }
            "n" -> {
              let new_black_knight_bitboard =
                from_position(Position(file: file_index, rank: rank_index))
              Ok(BoardBB(
                black_king_bitboard: acc.black_king_bitboard,
                black_queen_bitboard: acc.black_queen_bitboard,
                black_rook_bitboard: acc.black_rook_bitboard,
                black_bishop_bitboard: acc.black_bishop_bitboard,
                black_knight_bitboard: bitboard_or(
                  acc.black_knight_bitboard,
                  new_black_knight_bitboard,
                ),
                black_pawn_bitboard: acc.black_pawn_bitboard,
                white_king_bitboard: acc.white_king_bitboard,
                white_queen_bitboard: acc.white_queen_bitboard,
                white_rook_bitboard: acc.white_rook_bitboard,
                white_bishop_bitboard: acc.white_bishop_bitboard,
                white_knight_bitboard: acc.white_knight_bitboard,
                white_pawn_bitboard: acc.white_pawn_bitboard,
              ))
            }
            "p" -> {
              let new_black_pawn_bitboard =
                from_position(Position(file: file_index, rank: rank_index))
              Ok(BoardBB(
                black_king_bitboard: acc.black_king_bitboard,
                black_queen_bitboard: acc.black_queen_bitboard,
                black_rook_bitboard: acc.black_rook_bitboard,
                black_bishop_bitboard: acc.black_bishop_bitboard,
                black_knight_bitboard: acc.black_knight_bitboard,
                black_pawn_bitboard: bitboard_or(
                  acc.black_pawn_bitboard,
                  new_black_pawn_bitboard,
                ),
                white_king_bitboard: acc.white_king_bitboard,
                white_queen_bitboard: acc.white_queen_bitboard,
                white_rook_bitboard: acc.white_rook_bitboard,
                white_bishop_bitboard: acc.white_bishop_bitboard,
                white_knight_bitboard: acc.white_knight_bitboard,
                white_pawn_bitboard: acc.white_pawn_bitboard,
              ))
            }
            unknown_letter -> {
              Error("unknown letter board " <> unknown_letter)
            }
          }
        },
      )
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

fn parse_turn(turn_string: String) -> Result(Color, String) {
  case turn_string {
    "w" -> Ok(White)
    "b" -> Ok(Black)
    _ -> Error("Invalid turn string, must be 'w' or 'b'")
  }
}

fn parse_castling(castling_string: String) -> Result(CastlingStatus, String) {
  case string.length(castling_string) <= 4 {
    True -> {
      case
        string.split(castling_string, "")
        |> list.all(fn(part) {
          case part {
            "K" -> True
            "Q" -> True
            "k" -> True
            "q" -> True
            "-" -> True
            _ -> False
          }
        })
      {
        False -> Error("castling string contains unusual letters")
        True -> {
          let white_queenside_castling = string.contains(castling_string, "Q")
          let white_kingside_castling = string.contains(castling_string, "K")
          let black_queenside_castling = string.contains(castling_string, "q")
          let black_kingside_castling = string.contains(castling_string, "k")
          Ok(CastlingStatus(
            white_kingside: white_kingside_castling,
            white_queenside: white_queenside_castling,
            black_kingside: black_kingside_castling,
            black_queenside: black_queenside_castling,
          ))
        }
      }
    }
    False -> Error("castling string contains too many letters")
  }
}

fn parse_en_passant(
  en_passant_string: String,
) -> Result(Option(Position), String) {
  case en_passant_string {
    "-" -> Ok(None)
    _ -> {
      case string.length(en_passant_string) == 2 {
        True ->
          case string.split(en_passant_string, "") {
            [file_string, rank_string] -> {
              use file <- result.then(parse_file(file_string))
              use rank <- result.then(parse_rank(rank_string))
              Ok(Some(Position(file: file, rank: rank)))
            }
            _ -> Error("en passant missing full position")
          }
        False -> Error("Invalid en passant string")
      }
    }
  }
}

fn parse_file(file_string: String) -> Result(File, String) {
  case file_string {
    "a" -> Ok(A)
    "b" -> Ok(B)
    "c" -> Ok(C)
    "d" -> Ok(D)
    "e" -> Ok(E)
    "f" -> Ok(F)
    "g" -> Ok(G)
    "h" -> Ok(H)
    _ -> Error("Invalid file string")
  }
}

fn parse_rank(rank_string: String) -> Result(Rank, String) {
  case rank_string {
    "1" -> Ok(One)
    "2" -> Ok(Two)
    "3" -> Ok(Three)
    "4" -> Ok(Four)
    "5" -> Ok(Five)
    "6" -> Ok(Six)
    "7" -> Ok(Seven)
    "8" -> Ok(Eight)
    _ -> Error("Invalid rank string")
  }
}

fn parse_halfmove(halfmove_string: String) -> Result(HalfMove, String) {
  case string_to_int(halfmove_string) {
    None -> Error("Invalid halfmove string")
    Some(halfmove) -> Ok(halfmove)
  }
}

fn parse_fullmove(fullmove_string: String) -> Result(FullMove, String) {
  case string_to_int(fullmove_string) {
    None -> Error("Invalid fullmove string")
    Some(fullmove) -> Ok(fullmove)
  }
}

fn parse_digit(string: String) -> Option(Int) {
  case string {
    "0" -> Some(0)
    "1" -> Some(1)
    "2" -> Some(2)
    "3" -> Some(3)
    "4" -> Some(4)
    "5" -> Some(5)
    "6" -> Some(6)
    "7" -> Some(7)
    "8" -> Some(8)
    "9" -> Some(9)
    _ -> None
  }
}

fn string_to_int(string: String) -> Option(Int) {
  case string |> string.split("") {
    [digit] -> {
      parse_digit(digit)
    }
    [tenths_place_string, ones_place_string] -> {
      case parse_digit(tenths_place_string), parse_digit(ones_place_string) {
        Some(tenths_place_digit), Some(ones_place_digit) ->
          Some(tenths_place_digit * 10 + ones_place_digit)
        _, _ -> None
      }
    }
    [hundreds_place_string, tenths_place_string, ones_place_string] -> {
      case
        parse_digit(hundreds_place_string),
        parse_digit(tenths_place_string),
        parse_digit(ones_place_string)
      {
        Some(hundreds_place_digit),
          Some(tenths_place_digit),
          Some(ones_place_digit)
        ->
          Some(
            { hundreds_place_digit * 100 }
            + { tenths_place_digit * 10 }
            + ones_place_digit,
          )
        _, _, _ -> None
      }
    }
    _ -> None
  }
}

fn list_cons_some(list: List(a), maybe_new_head: Option(a)) -> List(a) {
  case maybe_new_head {
    None -> list
    Some(new_head) -> [new_head, ..list]
  }
}

fn list_map_and_somes(
  options: List(a),
  element_to_option: fn(a) -> Option(b),
) -> List(b) {
  list.filter_map(options, fn(element) {
    option.to_result(element_to_option(element), #())
  })
}

fn list_somes(options: List(Option(a))) -> List(a) {
  list.filter_map(options, fn(option) { option.to_result(option, #()) })
}

pub type MoveSanAndFenAfterMove {
  MoveSanAndFenAfterMove(move_san: MoveSan, fen_after_move: Fen)
}

pub fn legal_moves(fen: Fen) -> List(MoveSanAndFenAfterMove) {
  let board_pieces_for_turn = case fen.turn {
    Black -> board_pieces_black(fen.board)
    White -> board_pieces_white(fen.board)
  }
  board_pieces_for_turn
  |> list.flat_map(fn(piece) {
    case piece.kind {
      Pawn -> {
        // promotion kind knight (capture or not) is never even suggested to save compute
        // since it is only ever useful in obscure positions
        [
          case fen.en_passant {
            None -> None
            Some(moved_over_position) -> {
              let maybe_capture_position_left =
                position_offset_by(
                  piece.position,
                  x: -1,
                  y: position_offset_y_positive_if_white_negative_if_black(
                    1,
                    piece.color,
                  ),
                )
              let maybe_capture_position_right =
                position_offset_by(
                  piece.position,
                  x: 1,
                  y: position_offset_y_positive_if_white_negative_if_black(
                    1,
                    piece.color,
                  ),
                )
              case
                // there can only be one en passant-able capture
                { Some(moved_over_position) == maybe_capture_position_left }
                || { Some(moved_over_position) == maybe_capture_position_right }
              {
                False -> None
                True ->
                  Some(MoveSanEnPassant(
                    from: piece.position,
                    to: moved_over_position,
                  ))
              }
            }
          },
          // move forwards 1 (possibly promoting)
          case fen.turn {
            White ->
              case piece.position.rank {
                Seven -> {
                  let new_position =
                    Position(file: piece.position.file, rank: Eight)
                  case board_piece_at(fen.board, new_position) {
                    Some(_blocked) -> None
                    None ->
                      Some(MoveSanNormal(
                        moving_piece: piece.kind,
                        from: piece.position,
                        to: new_position,
                        capture: False,
                        promotion: Some(Queen),
                      ))
                  }
                }
                _ -> {
                  position_offset_by(piece.position, x: 0, y: 1)
                  |> option.then(fn(new_position) {
                    case board_piece_at(fen.board, new_position) {
                      Some(_blocked) -> None
                      None ->
                        Some(MoveSanNormal(
                          moving_piece: piece.kind,
                          from: piece.position,
                          to: new_position,
                          capture: False,
                          promotion: None,
                        ))
                    }
                  })
                }
              }
            Black ->
              case piece.position.rank {
                Two -> {
                  let new_position =
                    Position(file: piece.position.file, rank: One)
                  case board_piece_at(fen.board, new_position) {
                    Some(_blocked) -> None
                    None ->
                      Some(MoveSanNormal(
                        moving_piece: piece.kind,
                        from: piece.position,
                        to: new_position,
                        capture: False,
                        promotion: Some(Queen),
                      ))
                  }
                }
                _ -> {
                  position_offset_by(piece.position, x: 0, y: -1)
                  |> option.then(fn(new_position) {
                    case board_piece_at(fen.board, new_position) {
                      Some(_blocked) -> None
                      None ->
                        Some(MoveSanNormal(
                          moving_piece: piece.kind,
                          from: piece.position,
                          to: new_position,
                          capture: False,
                          promotion: None,
                        ))
                    }
                  })
                }
              }
          },
          // move forwards 2
          case fen.turn {
            Black ->
              case piece.position.rank {
                Two -> {
                  let new_position =
                    Position(rank: Four, file: piece.position.file)
                  let jumped_over_position =
                    Position(rank: Three, file: piece.position.file)
                  case
                    board_piece_at(fen.board, jumped_over_position),
                    board_piece_at(fen.board, new_position)
                  {
                    Some(_blocked), _ | _, Some(_blocked) -> None
                    None, None ->
                      Some(MoveSanNormal(
                        from: piece.position,
                        to: new_position,
                        promotion: None,
                        moving_piece: piece.kind,
                        capture: False,
                      ))
                  }
                }
                _ -> None
              }
            White ->
              case piece.position.rank {
                Seven -> {
                  let new_position =
                    Position(rank: Five, file: piece.position.file)
                  let jumped_over_position =
                    Position(rank: Six, file: piece.position.file)
                  case
                    board_piece_at(fen.board, jumped_over_position),
                    board_piece_at(fen.board, new_position)
                  {
                    Some(_blocked), _ | _, Some(_blocked) -> None
                    None, None ->
                      Some(MoveSanNormal(
                        from: piece.position,
                        to: new_position,
                        promotion: None,
                        moving_piece: piece.kind,
                        capture: False,
                      ))
                  }
                }
                _ -> None
              }
          },
          // capture opponent left
          case
            position_offset_by(
              piece.position,
              x: -1,
              y: position_offset_y_positive_if_white_negative_if_black(
                1,
                piece.color,
              ),
            )
          {
            None -> None
            Some(left_capture_position) ->
              case board_piece_at(fen.board, left_capture_position) {
                None -> None
                Some(captured_piece) -> {
                  case captured_piece.color == fen.turn {
                    True -> None
                    False ->
                      case fen.turn, piece.position.rank {
                        White, Seven | Black, Two ->
                          Some(MoveSanNormal(
                            moving_piece: piece.kind,
                            from: piece.position,
                            to: left_capture_position,
                            capture: True,
                            promotion: Some(Queen),
                          ))
                        _, _ ->
                          Some(MoveSanNormal(
                            moving_piece: piece.kind,
                            from: piece.position,
                            to: left_capture_position,
                            capture: True,
                            promotion: None,
                          ))
                      }
                  }
                }
              }
          },
          // capture opponent right
          case
            position_offset_by(
              piece.position,
              x: 1,
              y: position_offset_y_positive_if_white_negative_if_black(
                1,
                piece.color,
              ),
            )
          {
            None -> None
            Some(right_capture_position) ->
              case board_piece_at(fen.board, right_capture_position) {
                None -> None
                Some(captured_piece) ->
                  case captured_piece.color == fen.turn {
                    True -> None
                    False ->
                      case fen.turn, piece.position.rank {
                        White, Seven | Black, Two ->
                          Some(MoveSanNormal(
                            moving_piece: piece.kind,
                            from: piece.position,
                            to: right_capture_position,
                            capture: True,
                            promotion: Some(Queen),
                          ))
                        _, _ ->
                          Some(MoveSanNormal(
                            moving_piece: piece.kind,
                            from: piece.position,
                            to: right_capture_position,
                            capture: True,
                            promotion: None,
                          ))
                      }
                  }
              }
          },
        ]
        |> list_somes
      }
      Bishop ->
        [
          PositionOffset(x: -1, y: -1),
          PositionOffset(x: -1, y: 1),
          PositionOffset(x: 1, y: -1),
          PositionOffset(x: 1, y: 1),
        ]
        |> list.flat_map(fn(step_offset) {
          move_san_ray_until_capture_or_blocked_each_step_offset_by(
            fen.board,
            piece,
            from: piece.position,
            x: step_offset.x,
            y: step_offset.y,
          )
        })
      King ->
        [
          PositionOffset(x: 0, y: -1),
          PositionOffset(x: 0, y: 1),
          PositionOffset(x: 1, y: -1),
          PositionOffset(x: 1, y: 1),
          PositionOffset(x: -1, y: -1),
          PositionOffset(x: -1, y: 1),
          PositionOffset(x: 1, y: 0),
          PositionOffset(x: -1, y: 0),
        ]
        |> list_map_and_somes(fn(offset) {
          position_offset_by(piece.position, x: offset.x, y: offset.y)
        })
        |> list_map_and_somes(fn(end_position) {
          case board_piece_at(fen.board, end_position) {
            None ->
              Some(MoveSanNormal(
                piece.position,
                end_position,
                piece.kind,
                False,
                None,
              ))
            Some(destination_occupation_piece) ->
              case destination_occupation_piece.color != fen.turn {
                False -> None
                True ->
                  Some(MoveSanNormal(
                    piece.position,
                    end_position,
                    piece.kind,
                    True,
                    None,
                  ))
              }
          }
        })
        |> list_cons_some(case piece.color {
          Black ->
            case fen.castling.black_kingside {
              True ->
                case
                  board_piece_at(fen.board, Position(F, Eight)),
                  board_piece_at(fen.board, Position(G, Eight)),
                  board_is_check(
                    BoardBB(
                      ..fen.board,
                      black_king_bitboard: position_to_bitboard(Position(
                        F,
                        Eight,
                      )),
                    ),
                    for: fen.turn,
                  )
                {
                  None, None, False -> Some(MoveSanCastle(side: KingSide))
                  _, _, _ -> None
                }

              False -> None
            }
          White ->
            case fen.castling.white_kingside {
              True ->
                case
                  board_piece_at(fen.board, Position(F, One)),
                  board_piece_at(fen.board, Position(G, One)),
                  board_is_check(
                    BoardBB(
                      ..fen.board,
                      white_king_bitboard: position_to_bitboard(Position(F, One)),
                    ),
                    for: fen.turn,
                  )
                {
                  None, None, False -> Some(MoveSanCastle(side: KingSide))
                  _, _, _ -> None
                }
              False -> None
            }
        })
        |> list_cons_some(case piece.color {
          Black ->
            case fen.castling.black_queenside {
              True ->
                case
                  board_piece_at(fen.board, Position(B, Eight)),
                  board_piece_at(fen.board, Position(C, Eight)),
                  board_piece_at(fen.board, Position(D, Eight)),
                  board_is_check(
                    BoardBB(
                      ..fen.board,
                      black_king_bitboard: position_to_bitboard(Position(
                        D,
                        Eight,
                      )),
                    ),
                    for: fen.turn,
                  )
                {
                  None, None, None, False ->
                    Some(MoveSanCastle(side: QueenSide))
                  _, _, _, _ -> None
                }
              False -> None
            }
          White ->
            case fen.castling.white_queenside {
              True ->
                case
                  board_piece_at(fen.board, Position(B, One)),
                  board_piece_at(fen.board, Position(C, One)),
                  board_piece_at(fen.board, Position(D, One)),
                  board_is_check(
                    BoardBB(
                      ..fen.board,
                      white_king_bitboard: position_to_bitboard(Position(D, One)),
                    ),
                    for: fen.turn,
                  )
                {
                  None, None, None, False ->
                    Some(MoveSanCastle(side: QueenSide))
                  _, _, _, _ -> None
                }
              False -> None
            }
        })
      Knight ->
        [
          PositionOffset(x: -2, y: -1),
          PositionOffset(x: -2, y: 1),
          PositionOffset(x: 2, y: -1),
          PositionOffset(x: 2, y: 1),
          PositionOffset(x: 1, y: -2),
          PositionOffset(x: -1, y: -2),
          PositionOffset(x: 1, y: 2),
          PositionOffset(x: -1, y: 2),
        ]
        |> list_map_and_somes(fn(offset) {
          position_offset_by(piece.position, x: offset.x, y: offset.y)
        })
        |> list.map(fn(end_position) {
          MoveSanNormal(
            from: piece.position,
            to: end_position,
            promotion: None,
            moving_piece: piece.kind,
            capture: False,
          )
        })
        // TODO merge with above
        |> list_map_and_somes(fn(move_san) {
          case move_san {
            MoveSanCastle(side) -> Some(MoveSanCastle(side))
            MoveSanEnPassant(from, to) -> Some(MoveSanEnPassant(from, to))
            MoveSanNormal(
              from,
              destination_position,
              moving_piece,
              _,
              promotion,
            ) ->
              case board_piece_at(fen.board, destination_position) {
                None ->
                  Some(MoveSanNormal(
                    from,
                    destination_position,
                    moving_piece,
                    False,
                    promotion,
                  ))
                Some(destination_occupation_piece) ->
                  case destination_occupation_piece.color != fen.turn {
                    False -> None
                    True ->
                      Some(MoveSanNormal(
                        from,
                        destination_position,
                        moving_piece,
                        True,
                        promotion,
                      ))
                  }
              }
          }
        })
      Queen ->
        [
          PositionOffset(x: 0, y: -1),
          PositionOffset(x: -1, y: -1),
          PositionOffset(x: -1, y: 0),
          PositionOffset(x: -1, y: 1),
          PositionOffset(x: 1, y: -1),
          PositionOffset(x: 0, y: 1),
          PositionOffset(x: 1, y: 1),
        ]
        |> list.flat_map(fn(step_offset) {
          move_san_ray_until_capture_or_blocked_each_step_offset_by(
            fen.board,
            piece,
            from: piece.position,
            x: step_offset.x,
            y: step_offset.y,
          )
        })
      Rook ->
        [
          PositionOffset(x: 0, y: -1),
          PositionOffset(x: -1, y: 0),
          PositionOffset(x: 0, y: 1),
          PositionOffset(x: 1, y: 0),
        ]
        |> list.flat_map(fn(step_offset) {
          move_san_ray_until_capture_or_blocked_each_step_offset_by(
            fen.board,
            piece,
            from: piece.position,
            x: step_offset.x,
            y: step_offset.y,
          )
        })
    }
  })
  |> list_map_and_somes(fn(move_san) {
    let fen_after_move = fen_apply_move_san(fen, move_san)
    case board_is_check(fen_after_move.board, for: fen.turn) {
      False ->
        Some(MoveSanAndFenAfterMove(
          move_san: move_san,
          fen_after_move: fen_after_move,
        ))
      True -> None
    }
  })
}

pub fn fen_apply_move_san(fen: Fen, move: MoveSan) -> Fen {
  let new_fullmove = case fen.turn {
    Black -> fen.fullmove + 1
    White -> fen.fullmove
  }
  case move {
    MoveSanCastle(side) -> {
      let castle_rank = case fen.turn {
        Black -> Eight
        White -> One
      }
      Fen(
        board: case side {
          KingSide ->
            fen.board
            |> board_remove_piece_at(Position(rank: castle_rank, file: E))
            |> board_set_piece_at(
              Position(rank: castle_rank, file: G),
              Piece(color: fen.turn, kind: King),
            )
            |> board_remove_piece_at(Position(rank: castle_rank, file: H))
            |> board_set_piece_at(
              Position(rank: castle_rank, file: F),
              Piece(color: fen.turn, kind: Rook),
            )
          QueenSide ->
            fen.board
            |> board_remove_piece_at(Position(rank: castle_rank, file: E))
            |> board_set_piece_at(
              Position(rank: castle_rank, file: C),
              Piece(color: fen.turn, kind: King),
            )
            |> board_remove_piece_at(Position(rank: castle_rank, file: A))
            |> board_set_piece_at(
              Position(rank: castle_rank, file: D),
              Piece(color: fen.turn, kind: Rook),
            )
        },
        turn: fen.turn |> color_opposite,
        castling: fen.castling,
        en_passant: None,
        halfmove: fen.halfmove + 1,
        fullmove: new_fullmove,
      )
    }
    MoveSanEnPassant(from, to) ->
      case fen.en_passant {
        None ->
          // should never happen
          fen
        Some(pawn_position_to_capture_on_with_en_passant) ->
          Fen(
            board: fen.board
              |> board_set_piece_at(to, Piece(color: fen.turn, kind: Pawn))
              |> board_remove_piece_at(
                pawn_position_to_capture_on_with_en_passant,
              )
              |> board_remove_piece_at(from),
            turn: fen.turn |> color_opposite,
            castling: fen.castling,
            en_passant: None,
            halfmove: 0,
            fullmove: new_fullmove,
          )
      }
    MoveSanNormal(from, to, moving_piece, capture, promotion) ->
      case promotion {
        Some(promotion_piece_kind) ->
          Fen(
            board: fen.board
              |> board_set_piece_at(
                to,
                Piece(color: fen.turn, kind: promotion_piece_kind),
              )
              |> board_remove_piece_at(from),
            turn: fen.turn |> color_opposite,
            castling: fen.castling,
            en_passant: None,
            halfmove: 0,
            fullmove: new_fullmove,
          )
        None ->
          Fen(
            board: case capture {
              True ->
                fen.board
                |> board_set_piece_at(
                  to,
                  Piece(color: fen.turn, kind: moving_piece),
                )
                |> board_remove_piece_at(to)
                |> board_remove_piece_at(from)
              False ->
                fen.board
                |> board_set_piece_at(
                  to,
                  Piece(color: fen.turn, kind: moving_piece),
                )
                |> board_remove_piece_at(from)
            },
            turn: fen.turn |> color_opposite,
            castling: case fen.turn {
              White ->
                case moving_piece {
                  Rook ->
                    CastlingStatus(
                      white_kingside: fen.castling.white_kingside
                        && { from != Position(H, One) },
                      white_queenside: fen.castling.white_queenside
                        && { from != Position(A, One) },
                      black_kingside: fen.castling.black_kingside,
                      black_queenside: fen.castling.black_queenside,
                    )
                  King ->
                    CastlingStatus(
                      white_kingside: False,
                      white_queenside: False,
                      black_kingside: fen.castling.black_kingside,
                      black_queenside: fen.castling.black_queenside,
                    )
                  _ -> fen.castling
                }
              Black -> fen.castling
            },
            en_passant: case
              moving_piece,
              int.absolute_value(file_to_int(to.file) - file_to_int(from.file))
            {
              Pawn, 2 -> Some(to)
              _, _ -> None
            },
            halfmove: case moving_piece {
              Pawn -> 0
              _ -> fen.halfmove + 1
            },
            fullmove: new_fullmove,
          )
      }
  }
}

pub fn board_is_check(
  board: BoardBB,
  for king_color_to_check_for: Color,
) -> Bool {
  let assert [white_king_position, ..] =
    board.white_king_bitboard |> bitboard_occupied_positions
  let assert [black_king_position, ..] =
    board.black_king_bitboard |> bitboard_occupied_positions
  case king_color_to_check_for {
    Black ->
      position_attacked_by_pawn(
        black_king_position,
        pawn_color: White,
        pawn_bitboard: board.white_pawn_bitboard,
      )
      || position_attacked_by_king(
        black_king_position,
        king_position: white_king_position,
      )
      || position_attacked_by_knight(
        black_king_position,
        knight_bitboard: board.white_knight_bitboard,
      )
      || position_attacked_by_bishop(
        black_king_position,
        board,
        bishop_bitboard: board.white_bishop_bitboard,
      )
      || position_attacked_by_rook(
        black_king_position,
        board,
        rook_bitboard: board.white_rook_bitboard,
      )
      || position_attacked_by_queen(
        black_king_position,
        board,
        queen_bitboard: board.white_queen_bitboard,
      )
    White ->
      position_attacked_by_pawn(
        white_king_position,
        pawn_color: Black,
        pawn_bitboard: board.black_pawn_bitboard,
      )
      || position_attacked_by_king(
        white_king_position,
        king_position: black_king_position,
      )
      || position_attacked_by_knight(
        white_king_position,
        knight_bitboard: board.black_knight_bitboard,
      )
      || position_attacked_by_bishop(
        white_king_position,
        board,
        bishop_bitboard: board.black_bishop_bitboard,
      )
      || position_attacked_by_rook(
        white_king_position,
        board,
        rook_bitboard: board.black_rook_bitboard,
      )
      || position_attacked_by_queen(
        white_king_position,
        board,
        queen_bitboard: board.black_queen_bitboard,
      )
  }
}

fn position_attacked_by_pawn(
  position: Position,
  pawn_color pawn_color: Color,
  pawn_bitboard pawn_bitboard: Bitboard,
) -> Bool {
  let pawn_positions = pawn_bitboard |> bitboard_occupied_positions
  case
    position
    |> position_offset_by(
      x: 1,
      y: position_offset_y_positive_if_white_negative_if_black(
        1,
        pawn_color |> color_opposite,
      ),
    )
  {
    None -> False
    Some(top_right) -> pawn_positions |> list.contains(top_right)
  }
  || {
    case
      position
      |> position_offset_by(
        x: -1,
        y: position_offset_y_positive_if_white_negative_if_black(
          1,
          pawn_color |> color_opposite,
        ),
      )
    {
      None -> False
      Some(top_left) -> pawn_positions |> list.contains(top_left)
    }
  }
}

fn position_attacked_by_king(
  position: Position,
  king_position king_position: Position,
) -> Bool {
  case position |> position_offset_by(x: 1, y: 0) {
    None -> False
    Some(right) -> king_position == right
  }
  || {
    case position |> position_offset_by(x: 1, y: 1) {
      None -> False
      Some(top_right) -> king_position == top_right
    }
  }
  || {
    case position |> position_offset_by(x: 0, y: 1) {
      None -> False
      Some(top) -> king_position == top
    }
  }
  || {
    case position |> position_offset_by(x: -1, y: 1) {
      None -> False
      Some(top_left) -> king_position == top_left
    }
  }
  || {
    case position |> position_offset_by(x: -1, y: 0) {
      None -> False
      Some(left) -> king_position == left
    }
  }
  || {
    case position |> position_offset_by(x: -1, y: -1) {
      None -> False
      Some(bottom_left) -> king_position == bottom_left
    }
  }
  || {
    case position |> position_offset_by(x: 0, y: -1) {
      None -> False
      Some(bottom) -> king_position == bottom
    }
  }
  || {
    case position |> position_offset_by(x: 1, y: -1) {
      None -> False
      Some(bottom_right) -> king_position == bottom_right
    }
  }
}

fn position_attacked_by_knight(
  position: Position,
  knight_bitboard knight_bitboard: Bitboard,
) -> Bool {
  let knight_positions = knight_bitboard |> bitboard_occupied_positions
  case position |> position_offset_by(x: 2, y: 1) {
    None -> False
    Some(l_away) -> knight_positions |> list.contains(l_away)
  }
  || {
    case position |> position_offset_by(x: 1, y: 2) {
      None -> False
      Some(l_away) -> knight_positions |> list.contains(l_away)
    }
  }
  || {
    case position |> position_offset_by(x: -1, y: 2) {
      None -> False
      Some(l_away) -> knight_positions |> list.contains(l_away)
    }
  }
  || {
    case position |> position_offset_by(x: -2, y: 1) {
      None -> False
      Some(l_away) -> knight_positions |> list.contains(l_away)
    }
  }
  || {
    case position |> position_offset_by(x: -2, y: -1) {
      None -> False
      Some(l_away) -> knight_positions |> list.contains(l_away)
    }
  }
  || {
    case position |> position_offset_by(x: -1, y: -2) {
      None -> False
      Some(l_away) -> knight_positions |> list.contains(l_away)
    }
  }
  || {
    case position |> position_offset_by(x: 1, y: -2) {
      None -> False
      Some(l_away) -> knight_positions |> list.contains(l_away)
    }
  }
  || {
    case position |> position_offset_by(x: 2, y: -1) {
      None -> False
      Some(l_away) -> knight_positions |> list.contains(l_away)
    }
  }
}

fn position_attacked_by_bishop(
  position: Position,
  board: BoardBB,
  bishop_bitboard bishop_bitboard: Bitboard,
) -> Bool {
  let bishop_positions = bishop_bitboard |> bitboard_occupied_positions
  bishop_positions
  |> list.any(fn(knight_position) {
    [
      PositionOffset(x: 1, y: 1),
      PositionOffset(x: 1, y: -1),
      PositionOffset(x: -1, y: 1),
      PositionOffset(x: -1, y: -1),
    ]
    |> list.any(fn(ray_step_offset) {
      position_hit_by_ray(
        position_to_check: position,
        board: board,
        ray_from: knight_position,
        step_offset_x: ray_step_offset.x,
        step_offset_y: ray_step_offset.y,
      )
    })
  })
}

fn position_attacked_by_rook(
  position: Position,
  board: BoardBB,
  rook_bitboard rook_bitboard: Bitboard,
) -> Bool {
  let rook_positions = rook_bitboard |> bitboard_occupied_positions
  rook_positions
  |> list.any(fn(knight_position) {
    [
      PositionOffset(x: 1, y: 0),
      PositionOffset(x: -1, y: 0),
      PositionOffset(x: 0, y: 1),
      PositionOffset(x: 0, y: -1),
    ]
    |> list.any(fn(ray_step_offset) {
      position_hit_by_ray(
        position_to_check: position,
        board: board,
        ray_from: knight_position,
        step_offset_x: ray_step_offset.x,
        step_offset_y: ray_step_offset.y,
      )
    })
  })
}

fn position_attacked_by_queen(
  position: Position,
  board: BoardBB,
  queen_bitboard queen_bitboard: Bitboard,
) -> Bool {
  let queen_positions = queen_bitboard |> bitboard_occupied_positions
  queen_positions
  |> list.any(fn(knight_position) {
    [
      // straight
      PositionOffset(x: 1, y: 0),
      PositionOffset(x: -1, y: 0),
      PositionOffset(x: 0, y: 1),
      PositionOffset(x: 0, y: -1),
      // diagonal
      PositionOffset(x: 1, y: 1),
      PositionOffset(x: 1, y: -1),
      PositionOffset(x: -1, y: 1),
      PositionOffset(x: -1, y: -1),
    ]
    |> list.any(fn(ray_step_offset) {
      position_hit_by_ray(
        position_to_check: position,
        board: board,
        ray_from: knight_position,
        step_offset_x: ray_step_offset.x,
        step_offset_y: ray_step_offset.y,
      )
    })
  })
}

/// check if moving in the given steps reaches the given position
/// or is stopped by other pieces/walls
fn position_hit_by_ray(
  position_to_check position_to_check: Position,
  board board: BoardBB,
  ray_from ray_start: Position,
  step_offset_x step_offset_x: Int,
  step_offset_y step_offset_y: Int,
) -> Bool {
  case ray_start |> position_offset_by(step_offset_x, step_offset_y) {
    None -> False
    Some(next_ray_position) ->
      next_ray_position == position_to_check
      || {
        case board_piece_at(board, next_ray_position) {
          Some(_) -> False
          None ->
            position_hit_by_ray(
              position_to_check: position_to_check,
              board: board,
              ray_from: next_ray_position,
              step_offset_x: step_offset_x,
              step_offset_y: step_offset_y,
            )
        }
      }
  }
}

pub fn color_opposite(color: Color) -> Color {
  case color {
    Black -> White
    White -> Black
  }
}

pub fn rank_compare(a: Rank, b: Rank) -> order.Order {
  int.compare(rank_to_int(a), rank_to_int(b))
}

pub fn file_compare(a: File, b: File) -> order.Order {
  int.compare(file_to_int(a), file_to_int(b))
}

type PositionOffset {
  PositionOffset(x: Int, y: Int)
}

fn move_san_ray_until_capture_or_blocked_each_step_offset_by(
  board: BoardBB,
  piece: PiecePositioned,
  from from: Position,
  x x: Int,
  y y: Int,
) -> List(MoveSan) {
  case position_offset_by(from, x, y) {
    None -> []
    Some(next_position_in_ray) -> {
      case board_piece_at(board, next_position_in_ray) {
        None -> [
          MoveSanNormal(
            from: piece.position,
            to: next_position_in_ray,
            moving_piece: piece.kind,
            capture: False,
            promotion: None,
          ),
          ..move_san_ray_until_capture_or_blocked_each_step_offset_by(
            board,
            piece,
            from: next_position_in_ray,
            x: x,
            y: y,
          )
        ]
        Some(occupying_piece_at_next_position_in_ray) -> {
          case occupying_piece_at_next_position_in_ray.color == piece.color {
            True -> []
            False ->
              // capture 
              [
                MoveSanNormal(
                  from: piece.position,
                  to: next_position_in_ray,
                  moving_piece: piece.kind,
                  capture: True,
                  promotion: None,
                ),
              ]
          }
        }
      }
    }
  }
}

pub type PiecePositioned {
  PiecePositioned(color: Color, kind: PieceKind, position: Position)
}

pub fn board_pieces_white(board: BoardBB) -> List(PiecePositioned) {
  [
    board.white_king_bitboard
      |> bitboard_occupied_positions
      |> list.map(fn(position) {
        PiecePositioned(color: White, kind: King, position: position)
      }),
    board.white_queen_bitboard
      |> bitboard_occupied_positions
      |> list.map(fn(position) {
        PiecePositioned(color: White, kind: Queen, position: position)
      }),
    board.white_rook_bitboard
      |> bitboard_occupied_positions
      |> list.map(fn(position) {
        PiecePositioned(color: White, kind: Rook, position: position)
      }),
    board.white_bishop_bitboard
      |> bitboard_occupied_positions
      |> list.map(fn(position) {
        PiecePositioned(color: White, kind: Bishop, position: position)
      }),
    board.white_knight_bitboard
      |> bitboard_occupied_positions
      |> list.map(fn(position) {
        PiecePositioned(color: White, kind: Knight, position: position)
      }),
    board.white_pawn_bitboard
      |> bitboard_occupied_positions
      |> list.map(fn(position) {
        PiecePositioned(color: White, kind: Pawn, position: position)
      }),
  ]
  |> list.flatten
}

pub fn board_pieces_black(board: BoardBB) -> List(PiecePositioned) {
  [
    board.black_king_bitboard
      |> bitboard_occupied_positions
      |> list.map(fn(position) {
        PiecePositioned(color: Black, kind: King, position: position)
      }),
    board.black_queen_bitboard
      |> bitboard_occupied_positions
      |> list.map(fn(position) {
        PiecePositioned(color: Black, kind: Queen, position: position)
      }),
    board.black_rook_bitboard
      |> bitboard_occupied_positions
      |> list.map(fn(position) {
        PiecePositioned(color: Black, kind: Rook, position: position)
      }),
    board.black_bishop_bitboard
      |> bitboard_occupied_positions
      |> list.map(fn(position) {
        PiecePositioned(color: Black, kind: Bishop, position: position)
      }),
    board.black_knight_bitboard
      |> bitboard_occupied_positions
      |> list.map(fn(position) {
        PiecePositioned(color: Black, kind: Knight, position: position)
      }),
    board.black_pawn_bitboard
      |> bitboard_occupied_positions
      |> list.map(fn(position) {
        PiecePositioned(color: Black, kind: Pawn, position: position)
      }),
  ]
  |> list.flatten
}
