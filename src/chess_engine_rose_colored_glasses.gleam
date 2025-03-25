import fen.{
  type BoardBB, type Color, type Fen, type File, type MoveSan, type Piece,
  type PiecePositioned, type Position, type Rank, A, B, Bishop, Black, C, D, E,
  Eight, F, Fen, Five, Four, G, H, King, Knight, One, Pawn, Piece, Position,
  Queen, Rook, Seven, Six, Three, Two, White, bitboard_or, file_to_string,
  int_to_file, int_to_rank, legal_moves,
}
import gleam/float
import gleam/order

import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import mist

import wisp.{type Request, type Response}
import wisp/wisp_mist

pub fn main() {
  wisp.configure_logger()
  let secret_key_base = wisp.random_string(64)

  let assert Ok(_) =
    handle_request
    |> wisp_mist.handler(secret_key_base)
    |> mist.new
    |> mist.bind("0.0.0.0")
    |> mist.port(8000)
    |> mist.start_http

  process.sleep_forever()
}

fn handle_request(request: Request) -> Response {
  case wisp.path_segments(request) {
    ["move"] -> {
      use body <- wisp.require_string_body(request)
      let decode_result = json.parse(body, move_decoder())
      case decode_result {
        Error(_) -> wisp.bad_request()
        Ok(move) -> {
          let chosen_move = choose_move(move.0)
          wisp.ok() |> wisp.string_body(fen.move_san_to_string(chosen_move))
        }
      }
    }
    _ -> wisp.ok()
  }
}

fn move_decoder() {
  use fen_string <- decode.field("fen", decode.string)
  use turn <- decode.field("turn", player_decoder())
  // use failed_moves <- decode.field("failed_moves", decode.list(decode.string))
  case fen.fen_from_string(fen_string) {
    Ok(fen) -> decode.success(#(fen, turn))
    Error(error) -> {
      // why does gleam not have a decode.fail(error) without a default?, dafuq
      decode.failure(
        #(
          Fen(
            board: fen.BoardBB(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            turn: turn,
            castling: fen.CastlingStatus(
              white_kingside: True,
              white_queenside: True,
              black_kingside: True,
              black_queenside: True,
            ),
            en_passant: None,
            halfmove: 0,
            fullmove: 0,
          ),
          turn,
        ),
        error,
      )
    }
  }
}

fn player_decoder() {
  use player_string <- decode.then(decode.string)
  case player_string {
    "white" -> decode.success(White)
    "black" -> decode.success(Black)
    _ -> decode.failure(White, "Invalid player")
  }
}

//
fn order_invert(order: order.Order) -> order.Order {
  case order {
    order.Lt -> order.Gt
    order.Eq -> order.Eq
    order.Gt -> order.Lt
  }
}

fn choose_move(fen: Fen) -> MoveSan {
  case
    legal_moves(fen)
    |> list.map(fn(legal_move) {
      EvaluatedFenAndMove(
        evaluation: board_evaluate(legal_move.fen_after_move.board),
        fen_after_move: legal_move.fen_after_move,
        move_san: legal_move.move_san,
      )
    })
    |> list.sort(by: fn(choice_a, choice_b) {
      case fen.turn {
        White -> float.compare(choice_a.evaluation, choice_b.evaluation)
        Black ->
          order_invert(float.compare(choice_a.evaluation, choice_b.evaluation))
      }
    })
  {
    [best_move, ..] -> best_move.move_san
    [] ->
      // TODO should be impossible because the game server would have ended the game already
      fen.MoveSanCastle(fen.QueenSide)
  }
}

type EvaluatedFenAndMove {
  EvaluatedFenAndMove(
    evaluation: EvaluationForWhite,
    fen_after_move: Fen,
    move_san: MoveSan,
  )
}

/// the goal is shallowly evaluating long term "promise" of a position
/// without looking in the near future at at.
/// e.g. a far advanced pawn that has no chance of promoting in the near future is still extremely valuable
fn board_evaluate(board: BoardBB) -> EvaluationForWhite {
  let white_pieces = fen.board_pieces_white(board)
  let black_pieces = fen.board_pieces_black(board)

  let individual_piece_evaluation =
    list.append(white_pieces, black_pieces)
    |> list.fold(0.0, fn(so_far, piece) {
      evaluate_piece_positioned(piece) +. so_far
    })

  individual_piece_evaluation
}

fn evaluate_piece_positioned(piece: PiecePositioned) -> EvaluationForWhite {
  evaluation_negate_if_black(
    case piece.kind {
      Pawn -> 0.98
      Bishop -> 3.3
      King -> 0.0
      Knight -> 2.8
      Queen -> 9.1
      Rook -> 4.9
    },
    piece.color,
  )
}

fn evaluation_negate_if_black(eval: Float, color: Color) {
  case color {
    White -> eval
    Black -> eval |> float.negate
  }
}

type EvaluationForWhite =
  Float
