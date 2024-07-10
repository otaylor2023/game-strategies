open! Core
open! Async
open! Game_strategies_common_lib

module Exercises = struct
  (* Here are some functions which know how to create a couple different
     kinds of games *)
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
    print_endline print_str
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

  let rec find_winning_move
    (game : Game.t)
    (target_piece : Game.Piece.t)
    (curr_pos : Game.Position.t)
    ~(empty_pos : Game.Position.t option)
    ~(filled_left : int)
    ~(row_inc : int)
    ~(col_inc : int)
    =
    match
      ( filled_left
      , Core.is_some empty_pos
      , Game.Position.in_bounds curr_pos ~game_kind:game.game_kind )
    with
    | 0, true, _ -> empty_pos
    | _, _, false -> None
    | _, _, _ ->
      let curr_piece = Map.find game.board curr_pos in
      let empty_pos_and_filled_left_option =
        match curr_piece with
        | None when Core.is_some empty_pos -> None
        | None -> Some (Some curr_pos, filled_left)
        | Some curr_piece when not (Game.Piece.equal target_piece curr_piece)
          ->
          None
        | Some _curr_piece -> Some (empty_pos, filled_left - 1)
      in
      (match empty_pos_and_filled_left_option with
       | None -> None
       | Some (new_empty_pos, new_filled_left) ->
         let next_pos =
           { Game.Position.row = curr_pos.row + row_inc
           ; column = curr_pos.column + col_inc
           }
         in
         find_winning_move
           game
           target_piece
           next_pos
           ~empty_pos:new_empty_pos
           ~filled_left:new_filled_left
           ~row_inc
           ~col_inc)
  ;;

  (* then false else find_winning_move game target_piece {Game.Position.row = curr_pos.row + row_inc; column = curr_pos.column + col_inc} ~empty_left:(empty_left-1) ~filled_left ~row_inc ~col_inc *)

  (* Exercise 3 *)
  let winning_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
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
          find_winning_move
            game
            me
            position
            ~empty_pos:None
            ~filled_left:(win_length - 1)
            ~row_inc
            ~col_inc))
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
    ~(me : Game.Piece.t)
    (game : Game.t)
    =
    let winning_moves = winning_moves ~me game in
    match List.is_empty winning_moves with
    | false -> winning_moves
    | true ->
      let available_moves = available_moves game in
      let losing_moves = losing_moves ~me game in
      List.filter available_moves ~f:(fun move ->
        not (List.mem losing_moves move ~equal:Game.Position.equal))
  ;;

  let%expect_test "available_moves_that_do_not_immediately_lose_o" =
    let non_losing_moves =
      available_moves_that_do_not_immediately_lose ~me:Game.Piece.O near_win
    in
    print_s [%sexp (non_losing_moves : Game.Position.t list)];
    [%expect {| () |}];
    return ()
  ;;

  let%expect_test "available_moves_that_do_not_immediately_lose_x" =
    let non_losing_moves =
      available_moves_that_do_not_immediately_lose ~me:Game.Piece.O near_win
    in
    print_s [%sexp (non_losing_moves : Game.Position.t list)];
    [%expect {| () |}];
    return ()
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
           available_moves_that_do_not_immediately_lose ~me:piece non_win
         in
         print_s [%sexp (non_losing_moves : Game.Position.t list)];
         let non_losing_moves =
           available_moves_that_do_not_immediately_lose ~me:piece near_win
         in
         print_s [%sexp (non_losing_moves : Game.Position.t list)];
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
      ]
  ;;
end

let receive_request _client (query : Rpcs.Take_turn.Query.t) =
  print_s [%message "Query received" (query : Rpcs.Take_turn.Query.t)];
  let (response : Rpcs.Take_turn.Response.t) =
    { piece = query.you_play; position = { row = 0; column = 0 } }
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
