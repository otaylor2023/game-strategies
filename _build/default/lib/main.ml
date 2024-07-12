open! Core
open! Async
open! Game_strategies_common_lib

module Exercises = struct
  (* Here are some functions which know how to create a couple different
     kinds of games *)

  let win_value = 10000
  let empty_game = Game.empty Game.Game_kind.Tic_tac_toe

  let place_piece (game : Game.t) ~piece ~position : Game.t =
    let board = Map.set game.board ~key:position ~data:piece in
    { game with board }
  ;;

  let win_for_x =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  ;;

  let non_win =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  ;;

  let near_win =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  ;;

  let _omok1 =
    let open Game in
    Game.empty Game.Game_kind.Omok
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 1 }
  ;;

  let _game1 =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 1 }
  ;;

  let _trap1 =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
  ;;

  let trap2 =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  ;;

  (* |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 } *)

  (* |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 } *)

  (* |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 } *)

  (* |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 } *)
  (* |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 } *)

  (* |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 0 } *)
  (* |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 1 } *)

  (* |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 2 } *)
  (* |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 0 } *)

  (* |> place_piece ~piece:Piece.O ~position:{ Position.row = 10; column = 1 } *)

  (* |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 } *)

  (* |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 0 } *)

  let illegal_move =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 5; column = 2 }
  ;;

  let print_game (game : Game.t) : unit =
    let length = Game.Game_kind.board_length game.game_kind in
    let pieces =
      List.init length ~f:(fun r ->
        let piece_row =
          List.init length ~f:(fun c ->
            let (curr_pos : Game.Position.t) = { row = r; column = c } in
            let curr_piece = Map.find game.board curr_pos in
            match curr_piece with
            | Some piece -> Game.Piece.to_string piece
            | None -> " ")
        in
        let print_str = String.concat ~sep:" | " piece_row in
        print_str)
    in
    let print_str = String.concat ~sep:"\n---------\n" pieces in
    Core.print_endline print_str
  ;;

  let%expect_test "print_win_for_x" =
    print_game win_for_x;
    [%expect
      {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
    return ()
  ;;

  let%expect_test "print_non_win" =
    print_game non_win;
    [%expect
      {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
    return ()
  ;;

  (* Exercise 1 *)
  let available_moves (game : Game.t) : Game.Position.t list =
    (* Map.fold game.board ~init:[] ~f:(fun ~key ~data pos_list -> match data
       with | Game.Piece.X -> ) *)
    let length = Game.Game_kind.board_length game.game_kind in
    let all_coords =
      List.init length ~f:(fun row ->
        List.init length ~f:(fun column -> { Game.Position.row; column }))
      |> List.concat
    in
    (* let occupied_coords = Map.keys game.board in *)
    List.filter all_coords ~f:(fun coord -> not (Map.mem game.board coord))
  ;;

  let get_available_moves_near_pieces (game : Game.t) : Game.Position.t list =
    let available_moves = available_moves game in
    let filled_positions = Map.keys game.board in
    List.filter
      available_moves
      ~f:(fun { row = free_row; column = free_col } ->
        List.exists
          filled_positions
          ~f:(fun { row = filled_row; column = filled_col } ->
            let row_dist = filled_row - free_row in
            let col_dist = filled_col - free_col in
            let dist =
              sqrt
                (Float.of_int
                   ((row_dist * row_dist) + (col_dist * col_dist)))
            in
            Float.(
              Float.of_int (Game.Game_kind.win_length game.game_kind) >= dist)))
  ;;

  let%expect_test "available_spaces_none" =
    let moves = available_moves win_for_x in
    print_s [%sexp (moves : Game.Position.t list)];
    [%expect {| () |}];
    return ()
  ;;

  let%expect_test "available_spaces_some" =
    let moves = available_moves non_win in
    print_s [%sexp (moves : Game.Position.t list)];
    [%expect
      {| 
      (((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 1)) 
       ((row 1) (column 2)) ((row 2) (column 1))) |}];
    return ()
  ;;

  let rec evaluate_direction
    (game : Game.t)
    (target_piece : Game.Piece.t)
    (curr_position : Game.Position.t)
    ~win_length
    ~row_increment
    ~col_increment
    =
    (* let curr_position = {Game.Position.row = prev_position.row + row_increment; column = prev_position.column + col_increment} in *)
    if Int.equal win_length 0
    then true
    else (
      match Map.find game.board curr_position with
      | None -> false
      | Some curr_piece ->
        (match Game.Piece.equal curr_piece target_piece with
         | false -> false
         | true ->
           evaluate_direction
             game
             target_piece
             { Game.Position.row = curr_position.row + row_increment
             ; column = curr_position.column + col_increment
             }
             ~win_length:(win_length - 1)
             ~row_increment
             ~col_increment))
  ;;

  (* Exercise 2 *)
  let evaluate (game : Game.t) : Game.Evaluation.t =
    let win_length = Game.Game_kind.win_length game.game_kind in
    let filled_positions = Map.keys game.board in
    let increments = [ 0, 1; 1, 0; 1, -1; 1, 1 ] in
    let state =
      List.fold
        filled_positions
        ~init:Game.Evaluation.Game_continues
        ~f:(fun game_state curr_position ->
          (* check if row has already been explored *)
          (* check if its  the beginning of a win (just top left  or just top of row)*)
          let curr_piece = Map.find_exn game.board curr_position in
          let is_in_bounds =
            Game.Position.in_bounds ~game_kind:game.game_kind curr_position
          in
          match is_in_bounds, game_state with
          | false, _ -> Game.Evaluation.Illegal_move
          | true, Game.Evaluation.Illegal_move
          | true, Game.Evaluation.Game_over _ ->
            game_state
          | true, Game.Evaluation.Game_continues ->
            let is_win =
              List.exists
                increments
                ~f:(fun (row_increment, col_increment) ->
                  evaluate_direction
                    game
                    curr_piece
                    curr_position
                    ~win_length
                    ~row_increment
                    ~col_increment)
            in
            (match is_win with
             | true -> Game.Evaluation.Game_over { winner = Some curr_piece }
             | false -> Game.Evaluation.Game_continues))
    in
    match state with
    | Game.Evaluation.Game_continues ->
      let length = Game.Game_kind.board_length game.game_kind in
      let max_positions = length * length in
      if Int.equal (List.length filled_positions) max_positions
      then Game.Evaluation.Game_over { winner = None }
      else state
    | _ -> state
  ;;

  let rec find_win_path_omok
    (game : Game.t)
    (target_piece : Game.Piece.t)
    (curr_pos : Game.Position.t)
    ~(empty_list : Game.Position.t list)
    ~(empty_left : int)
    ~(filled_left : int)
    ~(row_inc : int)
    ~(col_inc : int)
    : Game.Position.t list
    =
    let is_max_empties = Int.equal empty_left 0 in
    let found_all_filled = Int.equal filled_left 0 in
    let is_over = found_all_filled && is_max_empties in
    let is_too_far = filled_left < 0 || empty_left < 0 in
    match is_over, is_too_far with
    | _, true -> []
    | true, _ -> empty_list
    | false, _ ->
      (match Game.Position.in_bounds curr_pos ~game_kind:game.game_kind with
       | false -> []
       | true ->
         (match Map.find game.board curr_pos with
          | None when is_max_empties -> []
          | Some curr_piece
            when not (Game.Piece.equal target_piece curr_piece) ->
            (match found_all_filled with true -> empty_list | false -> [])
          | curr_piece_option ->
            let new_empty_list, new_empty_left, new_filled_left =
              match curr_piece_option with
              | None -> curr_pos :: empty_list, empty_left - 1, filled_left
              | Some _curr_piece -> empty_list, empty_left, filled_left - 1
            in
            let next_pos =
              { Game.Position.row = curr_pos.row + row_inc
              ; column = curr_pos.column + col_inc
              }
            in
            find_win_path_omok
              game
              target_piece
              next_pos
              ~empty_list:new_empty_list
              ~empty_left:new_empty_left
              ~filled_left:new_filled_left
              ~row_inc
              ~col_inc))
  ;;

  let check_for_empty
    (game : Game.t)
    (num_to_check : int)
    (position : Game.Position.t)
    (row_inc : int)
    (col_inc : int)
    =
    let needed_moves =
      List.init num_to_check ~f:(fun _i ->
        { Game.Position.row = position.row - row_inc
        ; column = position.column - col_inc
        })
    in
    let all_empty =
      List.for_all needed_moves ~f:(fun pos ->
        match Map.find game.board pos with None -> true | _ -> false)
    in
    if all_empty then Some needed_moves else None
  ;;

  let get_winning_moves_omok
    ~(me : Game.Piece.t)
    (game : Game.t)
    (num_moves_needed : int)
    : Game.Position.t list list
    =
    let win_length = Game.Game_kind.win_length game.game_kind in
    let filled_positions = Map.keys game.board in
    List.concat_map filled_positions ~f:(fun position ->
      let increments = [ 0, 1; 1, 0; 1, -1; 1, 1 ] in
      List.filter_map increments ~f:(fun (row_inc, col_inc) ->
        let curr_piece = Map.find game.board position in
        match curr_piece with
        | Some curr_piece when not (Game.Piece.equal curr_piece me) -> None
        | _ ->
          let winning_moves =
            find_win_path_omok
              game
              me
              position
              ~empty_list:[]
              ~empty_left:num_moves_needed
              ~filled_left:(win_length - num_moves_needed)
              ~row_inc
              ~col_inc
          in
          (match List.length winning_moves with
           | 0 -> None
           | num_moves_found ->
             (match Int.equal num_moves_found num_moves_needed with
              | true -> Some winning_moves
              | false ->
                let extra_moves =
                  check_for_empty
                    game
                    (num_moves_needed - num_moves_found)
                    position
                    row_inc
                    col_inc
                in
                (match extra_moves with
                 | Some extra_moves ->
                   Some (List.append winning_moves extra_moves)
                 | None -> None)))))
  ;;

  let rec find_winning_move
    (game : Game.t)
    (target_piece : Game.Piece.t)
    (curr_pos : Game.Position.t)
    ~(empty_list : Game.Position.t list)
    ~(empty_left : int)
    ~(filled_left : int)
    ~(row_inc : int)
    ~(col_inc : int)
    =
    let is_max_empties = Int.equal empty_left 0 in
    let is_over = Int.equal filled_left 0 && is_max_empties in
    let is_too_far = filled_left < 0 || empty_left < 0 in
    match is_over, is_too_far with
    | _, true -> []
    | true, _ -> empty_list
    | false, _ ->
      (match Game.Position.in_bounds curr_pos ~game_kind:game.game_kind with
       | false -> []
       | true ->
         (match Map.find game.board curr_pos with
          | None when is_max_empties -> []
          | Some curr_piece
            when not (Game.Piece.equal target_piece curr_piece) ->
            []
          | curr_piece_option ->
            let new_empty_list, new_empty_left, new_filled_left =
              match curr_piece_option with
              | None -> curr_pos :: empty_list, empty_left - 1, filled_left
              | Some _curr_piece -> empty_list, empty_left, filled_left - 1
            in
            let next_pos =
              { Game.Position.row = curr_pos.row + row_inc
              ; column = curr_pos.column + col_inc
              }
            in
            find_winning_move
              game
              target_piece
              next_pos
              ~empty_list:new_empty_list
              ~empty_left:new_empty_left
              ~filled_left:new_filled_left
              ~row_inc
              ~col_inc))
  ;;

  (* then false else find_winning_move game target_piece {Game.Position.row = curr_pos.row + row_inc; column = curr_pos.column + col_inc} ~empty_left:(empty_left-1) ~filled_left ~row_inc ~col_inc *)

  (* Exercise 3 *)
  let get_winning_moves
    ~(me : Game.Piece.t)
    (game : Game.t)
    (num_moves : int)
    : Game.Position.t list list
    =
    let win_length = Game.Game_kind.win_length game.game_kind in
    let board_length = Game.Game_kind.board_length game.game_kind in
    let length_list = List.init board_length ~f:(fun i -> i) in
    let all_coords = List.cartesian_product length_list length_list in
    List.concat_map all_coords ~f:(fun (row, col) ->
      let position = { Game.Position.row; column = col } in
      let increments = [ 0, 1; 1, 0; 1, -1; 1, 1 ] in
      List.filter_map increments ~f:(fun (row_inc, col_inc) ->
        let curr_piece = Map.find game.board position in
        match curr_piece with
        | Some curr_piece when not (Game.Piece.equal curr_piece me) -> None
        | _ ->
          let winning_moves =
            find_winning_move
              game
              me
              position
              ~empty_list:[]
              ~empty_left:num_moves
              ~filled_left:(win_length - num_moves)
              ~row_inc
              ~col_inc
          in
          (match List.is_empty winning_moves with
           | true -> None
           | false -> Some winning_moves)))
  ;;

  let winning_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    let winning_moves = get_winning_moves ~me game 1 in
    match List.is_empty winning_moves with
    | true -> []
    | false ->
      List.concat winning_moves
      |> List.dedup_and_sort ~compare:Game.Position.compare
  ;;

  (* Exercise 4 *)
  let losing_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    let opponent_wins = winning_moves ~me:(Game.Piece.flip me) game in
    match List.length opponent_wins with
    | 0 -> []
    | len ->
      let all_moves = available_moves game in
      if len > 1
      then all_moves
      else
        List.filter all_moves ~f:(fun move ->
          not (List.mem opponent_wins move ~equal:Game.Position.equal))
  ;;

  (* will choose a winning move if possible. skipping a win is basically losing *)
  let available_moves_that_do_not_immediately_lose
    (game : Game.t)
    ~(me : Game.Piece.t)
    ~(nearby : bool)
    =
    let winning_moves = winning_moves ~me game in
    match List.is_empty winning_moves with
    | false -> winning_moves
    | true ->
      let available_moves =
        if nearby
        then get_available_moves_near_pieces game
        else available_moves game
      in
      let losing_moves = losing_moves ~me game in
      List.filter available_moves ~f:(fun move ->
        not (List.mem losing_moves move ~equal:Game.Position.equal))
  ;;

  let%expect_test "available_moves_that_do_not_immediately_lose_o" =
    let non_losing_moves =
      available_moves_that_do_not_immediately_lose
        near_win
        ~me:Game.Piece.O
        ~nearby:false
    in
    print_s [%sexp (non_losing_moves : Game.Position.t list)];
    [%expect {| () |}];
    return ()
  ;;

  let%expect_test "available_moves_that_do_not_immediately_lose_x" =
    let non_losing_moves =
      available_moves_that_do_not_immediately_lose
        ~me:Game.Piece.X
        near_win
        ~nearby:false
    in
    print_s [%sexp (non_losing_moves : Game.Position.t list)];
    [%expect {| (((row 0) (column 2)) ((row 2) (column 0))) |}];
    return ()
  ;;

  let calculate_score (game : Game.t) (me : Game.Piece.t) =
    let length = Game.Game_kind.win_length game.game_kind in
    let length_list = List.init (length - 1) ~f:(fun i -> i + 1) in
    List.fold length_list ~init:0 ~f:(fun score num_empty ->
      let value = 10 * (length - num_empty) in
      let num_win_patterns =
        match game.game_kind with
        | Game.Game_kind.Tic_tac_toe ->
          List.length (get_winning_moves ~me game num_empty)
        | Game.Game_kind.Omok ->
          List.length (get_winning_moves_omok ~me game num_empty)
      in
      score + (value * num_win_patterns))
  ;;

  let get_player_score (game : Game.t) (me : Game.Piece.t) =
    (* match evaluate game with
       | Game.Evaluation.Game_over winner ->
       (match winner.winner with
       | Some winner when Game.Piece.equal me winner -> win_value
       | Some _winner -> -win_value
       | None -> win_value / 2)
       | _ -> *)
    let is_ttt =
      Game.Game_kind.equal game.game_kind Game.Game_kind.Tic_tac_toe
    in
    (* if (List.is_empty (available_moves game)) then win_value/2 else *)
    (* match current_turn with
       | true ->
       (match List.length (winning_moves ~me game) with
       | num_wins when num_wins > 0 -> win_value * num_wins
       | _ -> if is_ttt then 0 else calculate_score game me)
       | false ->
       (match
       List.is_empty
       (available_moves_that_do_not_immediately_lose ~me game)
       with
       | true ->
       -win_value
       * List.length (winning_moves ~me:(Game.Piece.flip me) game)
       | false -> if is_ttt then 0 else calculate_score game me) *)
    match List.length (winning_moves ~me game) with
    | num_wins when num_wins > 0 -> win_value * num_wins
    | _ ->
      (match List.length (winning_moves ~me:(Game.Piece.flip me) game) with
       | opponent_wins when opponent_wins > 0 -> -opponent_wins * win_value
       | _ ->
         if is_ttt
         then 0
         else
           calculate_score game me
           - calculate_score game (Game.Piece.flip me))
  ;;

  let score (game : Game.t) (me : Game.Piece.t) =
    let me_score = get_player_score game me in
    (* let opponent_score = get_player_score game (Game.Piece.flip me) false in *)
    (* print_s
       [%message
        (me : Game.Piece.t)
          (me_score : int)
          (Game.Piece.flip me : Game.Piece.t)
          (opponent_score : int)]; *)
    me_score
  ;;

  let make_move
    (game : Game.t)
    (piece : Game.Piece.t)
    (position : Game.Position.t)
    =
    game |> place_piece ~piece ~position
  ;;

  let available_moves_nearby_if_possible (game : Game.t) (me : Game.Piece.t) =
    let nearby_next_moves =
      available_moves_that_do_not_immediately_lose game ~me ~nearby:true
    in
    match List.is_empty nearby_next_moves with
    | true ->
      available_moves_that_do_not_immediately_lose game ~me ~nearby:false
    | false -> nearby_next_moves
  ;;

  let next_moves_exist
    (game : Game.t)
    ~(current_piece : Game.Piece.t)
    (total_moves_left : int)
    =
    match game.game_kind with
    | Game.Game_kind.Tic_tac_toe ->
      let next_moves =
        available_moves_that_do_not_immediately_lose
          game
          ~me:current_piece
          ~nearby:false
      in
      let immediate_wins = winning_moves ~me:current_piece game in
      let should_finish =
        Int.equal total_moves_left 0
        || (not (List.is_empty immediate_wins))
        || List.is_empty next_moves
      in
      (match should_finish with
       | true ->
         (* Core.print_s
            [%message
             (current_piece : Game.Piece.t)
               (Int.equal total_moves_left 0 : bool)
               (not (List.is_empty immediate_wins) : bool)
               (List.is_empty next_moves : bool)]; *)
         None
       | false -> Some next_moves)
    | Game.Game_kind.Omok ->
      (match Int.equal total_moves_left 0 with
       | true -> None
       | false ->
         let next_moves =
           available_moves_nearby_if_possible game current_piece
         in
         (match List.is_empty next_moves with
          | true -> None
          | false -> Some next_moves))
  ;;

  let rec get_game_score
    (game : Game.t)
    ~(original_piece : Game.Piece.t)
    ~(current_piece : Game.Piece.t)
    (total_moves_left : int)
    =
    let next_moves_opt =
      next_moves_exist game ~current_piece total_moves_left
    in
    match next_moves_opt with
    | None ->
      let og_score = score game current_piece in
      let score =
        if Game.Piece.equal original_piece current_piece
        then og_score
        else -og_score
      in
      let mult_score = score * (total_moves_left + 1) in
      (* print_game game; *)
      (* Core.print_s
         [%message
          (og_score : int)
            (score : int)
            (mult_score : int)
            (total_moves_left : int)]; *)
      mult_score
    | Some next_moves ->
      (* print_s
         [%message
          (current_piece : Game.Piece.t) (next_moves : Game.Position.t list)]; *)
      (* print_game game; *)
      let score_list =
        List.map next_moves ~f:(fun next_move ->
          let new_game = make_move game current_piece next_move in
          (* print_game new_game; *)
          let score =
            get_game_score
              new_game
              ~original_piece
              ~current_piece:(Game.Piece.flip current_piece)
              (total_moves_left - 1)
          in
          (* print_s
             [%message
              (next_move : Game.Position.t)
                (total_moves_left - 1 : int)
                (score : int)]; *)
          score)
      in
      let is_maximizing = Game.Piece.equal original_piece current_piece in
      (* Core.print_s [%message (is_maximizing : bool) (score_list : int list)]; *)
      (match is_maximizing with
       | true ->
         (match List.max_elt score_list ~compare:Int.compare with
          | Some score -> score
          (* no non-losing moves min *)
          | None -> -win_value)
       | false ->
         (match List.min_elt score_list ~compare:Int.compare with
          | Some score -> score
          (* no non-losing moves max *)
          | None -> win_value))
  ;;

  let get_best_move (game : Game.t) (me : Game.Piece.t) (depth : int) =
    let next_moves = available_moves_nearby_if_possible game me in
    (* print_game game; *)
    (* print_s
       [%message
        "initial"
          (depth : int)
          (me : Game.Piece.t)
          (next_moves : Game.Position.t list)]; *)
    let scores_and_moves =
      List.map next_moves ~f:(fun next_move ->
        (* print_s [%message "next"]; *)
        let new_game = make_move game me next_move in
        (* print_game new_game; *)
        (* print_s [%message (depth : int)]; *)
        let score =
          get_game_score
            new_game
            ~original_piece:me
            ~current_piece:(Game.Piece.flip me)
            (depth - 1)
        in
        (* print_s [%message (score : int)]; *)
        score, next_move)
    in
    let best_score_and_move =
      List.max_elt
        scores_and_moves
        ~compare:(fun (score1, _move1) (score2, _move2) ->
          Int.compare score1 score2)
    in
    match best_score_and_move with
    | Some (_score, move) -> Some move
    | None -> None
  ;;

  let choose_next_move (game : Game.t) (me : Game.Piece.t) depth =
    match game.game_kind, List.is_empty (Map.keys game.board) with
    | Game.Game_kind.Omok, true ->
      let coord = Int.of_float (Random.float_range 5. 10.) in
      { Game.Position.row = coord; column = coord }
    | _, _ ->
      let next_move = get_best_move game me depth in
      (match next_move with
       | Some move -> move
       | None ->
         Core.print_s [%message "no best move"];
         let available_moves = available_moves game in
         (match List.is_empty available_moves with
          | true ->
            Core.print_s [%message (evaluate game : Game.Evaluation.t)];
            print_game game;
            List.random_element_exn available_moves
          | false -> List.random_element_exn available_moves))
  ;;

  let exercise_one =
    Command.async
      ~summary:"Exercise 1: Where can I move?"
      (let%map_open.Command () = return () in
       fun () ->
         let moves = available_moves win_for_x in
         print_s [%sexp (moves : Game.Position.t list)];
         let moves = available_moves non_win in
         print_s [%sexp (moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_two =
    Command.async
      ~summary:"Exercise 2: Is the game over?"
      (let%map_open.Command () = return () in
       fun () ->
         let evaluation = evaluate win_for_x in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         let evaluation = evaluate non_win in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         let evaluation = evaluate illegal_move in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         return ())
  ;;

  let piece_flag =
    let open Command.Param in
    flag
      "piece"
      (required (Arg_type.create Game.Piece.of_string))
      ~doc:
        ("PIECE "
         ^ (Game.Piece.all
            |> List.map ~f:Game.Piece.to_string
            |> String.concat ~sep:", "))
  ;;

  let exercise_three =
    Command.async
      ~summary:"Exercise 3: Is there a winning move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let winning_moves = winning_moves ~me:piece non_win in
         print_s [%sexp (winning_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_four =
    Command.async
      ~summary:"Exercise 4: Is there a losing move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let losing_moves = losing_moves ~me:piece non_win in
         print_s [%sexp (losing_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_five =
    Command.async
      ~summary:"Exercise 5: Non losing moves"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let non_losing_moves =
           available_moves_that_do_not_immediately_lose
             ~me:piece
             non_win
             ~nearby:false
         in
         print_s [%sexp (non_losing_moves : Game.Position.t list)];
         let non_losing_moves =
           available_moves_that_do_not_immediately_lose
             ~me:piece
             near_win
             ~nearby:false
         in
         print_s [%sexp (non_losing_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_six =
    Command.async
      ~summary:"Exercise 6: Score"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let board = trap2 in
         print_game board;
         let best_move = choose_next_move board piece 9 in
         let new_board = place_piece board ~piece ~position:best_move in
         Core.print_s [%sexp (best_move : Game.Position.t)];
         print_game new_board;
         (* let moves = find_winning_move
              game1
              piece
              {Game.Position.row = 0; column = 1}
              ~empty_list:[]
              ~empty_left:2
              ~filled_left:(win_length - 1)
              ~row_inc
              ~col_inc *)
         (* let winning_moves = get_winning_moves game1 ~me:piece 2 in
            print_s [%message (winning_moves : Game.Position.t list list)]; *)
         let score = score board piece in
         Core.print_s [%sexp (score : int)];
         return ())
  ;;

  let exercise_seven =
    Command.async
      ~summary:"Exercise 7: TicTacToe against self"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let list = List.init 9 ~f:(fun i -> i) in
         let _winner =
           List.fold
             list
             ~init:(empty_game, piece)
             ~f:(fun (board, piece) _num ->
               let game_state = evaluate board in
               match game_state with
               | Game_over _ | Illegal_move ->
                 Core.print_s [%message (game_state : Game.Evaluation.t)];
                 let _stop = failwith "Game is over" in
                 board, piece
               | _ ->
                 Core.print_s [%message "BOARD: "];
                 print_game board;
                 let best_move = choose_next_move board piece 9 in
                 make_move board piece best_move, Game.Piece.flip piece)
         in
         (* print_s [%sexp (best_move : Game.Position.t option)]; *)
         return ())
  ;;

  let exercise_eight =
    Command.async
      ~summary:"Exercise 8: Omok"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let best_move = choose_next_move trap2 piece 2 in
         (* make_move omok1 piece best_move *)
         print_s [%sexp (best_move : Game.Position.t)];
         return ())
  ;;

  let exercise_nine =
    Command.async
      ~summary:"Exercise 9: Omok against self"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         Core.print_s [%message "starting"];
         let list = List.init 228 ~f:(fun i -> i) in
         Core.print_s [%message (List.length list : int)];
         let _winner =
           List.fold
             list
             ~init:(Game.empty Game.Game_kind.Omok, piece)
             ~f:(fun (board, piece) _num ->
               let game_state = evaluate board in
               match game_state with
               | Game_over _ | Illegal_move ->
                 Core.print_s [%message (game_state : Game.Evaluation.t)];
                 let _stop = 10 / 0 in
                 board, piece
               | _ ->
                 Core.print_s [%message "BOARD: "];
                 let best_move = choose_next_move board piece 2 in
                 Core.print_s [%message (best_move : Game.Position.t)];
                 let new_board, next_piece =
                   make_move board piece best_move, Game.Piece.flip piece
                 in
                 print_game new_board;
                 (* let _str = Stdlib.read_line () in *)
                 new_board, next_piece)
         in
         (* print_s [%sexp (best_move : Game.Position.t option)]; *)
         return ())
  ;;

  let exercise_ten =
    Command.async
      ~summary:"Exercise 10: Omok against random bot"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         Core.print_s [%message "starting"];
         let list = List.init 228 ~f:(fun i -> i) in
         Core.print_s [%message (List.length list : int)];
         let _winner =
           List.fold
             list
             ~init:(Game.empty Game.Game_kind.Omok)
             ~f:(fun board _num ->
               Core.print_s [%message "BOARD: "];
               let best_move = choose_next_move board piece 1 in
               Core.print_s [%message (best_move : Game.Position.t)];
               let new_board = make_move board piece best_move in
               print_game new_board;
               let _str = Stdlib.read_line () in
               let next_move =
                 List.random_element_exn (available_moves new_board)
               in
               let next_board =
                 make_move new_board (Game.Piece.flip piece) next_move
               in
               print_game next_board;
               let _str = Stdlib.read_line () in
               next_board)
         in
         (* print_s [%sexp (best_move : Game.Position.t option)]; *)
         return ())
  ;;

  let exercise_eleven =
    Command.async
      ~summary:"Exercise 11: Helper function working"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         Core.print_s [%message "starting"];
         let game = trap2 in
         print_game game;
         let next_moves_exist =
           next_moves_exist game ~current_piece:piece 3
         in
         (* let score = score game piece in
            Core.print_s [%sexp (score : int)]; *)
         Core.print_s
           [%sexp (next_moves_exist : Game.Position.t list option)];
         return ())
  ;;

  let command =
    Command.group
      ~summary:"Exercises"
      [ "one", exercise_one
      ; "two", exercise_two
      ; "three", exercise_three
      ; "four", exercise_four
      ; "five", exercise_five
      ; "six", exercise_six
      ; "seven", exercise_seven
      ; "eight", exercise_eight
      ; "nine", exercise_nine
      ; "ten", exercise_ten
      ; "eleven", exercise_eleven
      ]
  ;;
end

let receive_request _client (query : Rpcs.Take_turn.Query.t) =
  print_s [%message "Query received" (query : Rpcs.Take_turn.Query.t)];
  match query.game.game_kind with
  | Game.Game_kind.Tic_tac_toe ->
    let (response : Rpcs.Take_turn.Response.t) =
      { piece = query.you_play
      ; position = Exercises.choose_next_move query.game query.you_play 9
      }
    in
    return response
  | Game.Game_kind.Omok ->
    let (response : Rpcs.Take_turn.Response.t) =
      { piece = query.you_play
      ; position = Exercises.choose_next_move query.game query.you_play 1
      }
    in
    return response
;;

let command_play =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
     (* and _controller = flag "-controller" (required host_and_port) ~doc:"_
        host_and_port of controller" *)
     and port = flag "-port" (required int) ~doc:"_ port to listen on" in
     fun () ->
       let implementations =
         Rpc.Implementations.create_exn
           ~on_unknown_rpc:`Close_connection
           ~implementations:
             [ Rpc.Rpc.implement Rpcs.Take_turn.rpc receive_request ]
       in
       let%bind server =
         Rpc.Connection.serve
           ~implementations
           ~initial_connection_state:(fun _client_identity _client_addr ->
             (* This constructs the "client" values which are passed to the
                implementation function above. We're just using unit for
                now. *)
             ())
           ~where_to_listen:(Tcp.Where_to_listen.of_port port)
           ()
       in
       print_s [%message (port : int)];
       Tcp.Server.close_finished server
     (* We should start listing on the supplied [port], ready to handle
        incoming queries for [Take_turn] and [Game_over]. We should also
        connect to the controller and send a [Start_game] to initiate the
        game. *))
;;

let command =
  Command.group
    ~summary:"Game Strategies"
    [ "play", command_play; "exercises", Exercises.command ]
;;
