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
    List.filter all_coords ~f:(fun coord ->
      not (Map.mem game.board coord))
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

  (* Exercise 2 *)
  let evaluate (game : Game.t) : Game.Evaluation.t =
    let length = Game.Game_kind.board_length game.game_kind in
    let length_list = List.init length ~f:(fun value -> value) in
    let filled_positions = Map.keys game.board in
    List.iter filled_positions ~f:(fun {row = curr_row; column = curr_col} ->
      (* check if row has already been explored *)
        (* check if its  the beginning of a win (just top left  or just top of row)*)
        let curr_piece = Map.find_exn game.board {row = curr_row; column = curr_col} in
        let row_win = List.fold_until length_list ~finish:(fun value -> value) ~init:true ~f:(fun row ->
          match Map.find game.board {row; curr_col} with 
          | Some piece when Game.Piece.equal curr_piece piece -> Continue_or_stop.Continue true
          | None | Some _ -> Continue_or_stop.Stop false
          
          )
      );
    Game.Evaluation.Game_continues
  ;;

  (* Exercise 3 *)
  let winning_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    ignore me;
    ignore game;
    failwith "Implement me!"
  ;;

  (* Exercise 4 *)
  let losing_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    ignore me;
    ignore game;
    failwith "Implement me!"
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
         let evaluation = evaluate win_for_x in
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

  let command =
    Command.group
      ~summary:"Exercises"
      [ "one", exercise_one
      ; "two", exercise_two
      ; "three", exercise_three
      ; "four", exercise_four
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
