import board.{
  A, E, Four, H, KingSide, MoveSanCastle, MoveSanNormal, One, Pawn, Position,
  PositionSan, Queen, QueenSide, Rook, Three, move_san_from_string,
}
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn move_san_from_string_test() {
  let assert Ok(move) = move_san_from_string("e4")

  move
  |> should.equal(MoveSanNormal(
    moving_piece: Pawn,
    from: None,
    to: Position(file: E, rank: Four),
    capture: False,
    promotion: None,
    maybe_check_or_checkmate: None,
  ))

  let assert Ok(move) = move_san_from_string("R1a3")

  move
  |> should.equal(MoveSanNormal(
    moving_piece: Rook,
    from: Some(PositionSan(file: None, rank: Some(One))),
    to: Position(file: A, rank: Three),
    capture: False,
    promotion: None,
    maybe_check_or_checkmate: None,
  ))

  let assert Ok(move) = move_san_from_string("Rxa3")
  move
  |> should.equal(MoveSanNormal(
    moving_piece: Rook,
    from: None,
    to: Position(file: A, rank: Three),
    capture: True,
    promotion: None,
    maybe_check_or_checkmate: None,
  ))

  let assert Ok(move) = move_san_from_string("Qh4e1")
  move
  |> should.equal(MoveSanNormal(
    moving_piece: Queen,
    from: Some(PositionSan(file: Some(H), rank: Some(Four))),
    to: Position(file: E, rank: One),
    capture: False,
    promotion: None,
    maybe_check_or_checkmate: None,
  ))

  let assert Ok(move) = move_san_from_string("0-0")
  move
  |> should.equal(MoveSanCastle(side: KingSide, maybe_check_or_checkmate: None))

  let assert Ok(move) = move_san_from_string("0-0-0")
  move
  |> should.equal(MoveSanCastle(side: QueenSide, maybe_check_or_checkmate: None))
}
