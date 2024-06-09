-module(ec).

-export([esc/0, esc/1, home/0, move/2, right/1, up/1, left/1, down/1, next/1, prev/1,
         col/1, pos/0]).
-export([erase_in_display/0, erase_cursor_end_screen/0, erase_cursor_begin_screen/0,
         erase_screen/0]).
-export([erase_in_line/0, erase_cursor_end_line/0, erase_cursor_begin_line/0,
         erase_line/0]).

-include_lib("eunit/include/eunit.hrl").

-spec esc() -> 16#1B.
esc() ->
  16#1B.

esc(Terms) ->
  [esc(), Terms].

escape_test() ->
  ?assertEqual($\e, esc()).

%% Cursor commands
-spec home() -> iodata().
home() ->
  [esc(), $[, $H].

home_test() ->
  ?assertEqual([27, $[, $H], lists:flatten(home())).

-spec move(non_neg_integer(), non_neg_integer()) -> iodata().
move(X, Y) ->
  esc([$[, integer_to_list(Y), $;, integer_to_list(X), $H]).

move_test() ->
  ?assertEqual([27, $[, $2, $;, $1, $0, $H], lists:flatten(move(10, 2))).

-spec up(non_neg_integer()) -> iodata().
up(A) ->
  esc([$[, integer_to_list(A), $A]).

up_test() ->
  ?assertEqual([27, $[, $1, $0, $A], lists:flatten(up(10))).

-spec down(non_neg_integer()) -> iodata().
down(A) ->
  esc([$[, integer_to_list(A), $B]).

down_test() ->
  ?assertEqual([27, $[, $1, $0, $B], lists:flatten(down(10))).

-spec left(non_neg_integer()) -> iodata().
left(A) ->
  esc([$[, integer_to_list(A), $C]).

left_test() ->
  ?assertEqual([27, $[, $1, $0, $C], lists:flatten(left(10))).

-spec right(non_neg_integer()) -> iodata().
right(A) ->
  esc([$[, integer_to_list(A), $D]).

right_test() ->
  ?assertEqual([27, $[, $1, $0, $D], lists:flatten(right(10))).

-spec next(non_neg_integer()) -> iodata().
next(A) ->
  esc([$[, integer_to_list(A), $E]).

next_test() ->
  ?assertEqual([27, $[, $1, $0, $E], lists:flatten(next(10))).

-spec prev(non_neg_integer()) -> iodata().
prev(A) ->
  esc([$[, integer_to_list(A), $F]).

prev_test() ->
  ?assertEqual([27, $[, $1, $0, $F], lists:flatten(prev(10))).

-spec col(non_neg_integer()) -> iodata().
col(A) ->
  esc([$[, integer_to_list(A), $G]).

col_test() ->
  ?assertEqual([27, $[, $1, $0, $G], lists:flatten(col(10))).

-spec pos() -> iodata().
pos() ->
  esc([$[, $6, $n]).

pos_test() ->
  ?assertEqual([27, $[, $6, $n], lists:flatten(pos())).

%% Erase
-spec erase_in_display() -> iodata().
erase_in_display() ->
  esc([$[, $J]).

erase_in_display_test() ->
  ?assertEqual([27, $[, $J], lists:flatten(erase_in_display())).

-spec erase_cursor_end_screen() -> iodata().
erase_cursor_end_screen() ->
  esc([$[, $0, $J]).

erase_cursor_end_screen_test() ->
  ?assertEqual([27, $[, $0, $J], lists:flatten(erase_cursor_end_screen())).

-spec erase_cursor_begin_screen() -> iodata().
erase_cursor_begin_screen() ->
  esc([$[, $1, $J]).

erase_cursor_begin_test() ->
  ?assertEqual([27, $[, $1, $J], lists:flatten(erase_cursor_begin_screen())).

-spec erase_screen() -> iodata().
erase_screen() ->
  esc([$[, $2, $J]).

erase_screen_test() ->
  ?assertEqual([27, $[, $2, $J], lists:flatten(erase_screen())).

-spec erase_in_line() -> iodata().
erase_in_line() ->
  esc([$[, $K]).

erase_in_line_test() ->
  ?assertEqual([27, $[, $K], lists:flatten(erase_in_line())).

-spec erase_cursor_end_line() -> iodata().
erase_cursor_end_line() ->
  esc([$[, $0, $K]).

erase_cursor_line_end_test() ->
  ?assertEqual([27, $[, $0, $K], lists:flatten(erase_cursor_end_line())).

-spec erase_cursor_begin_line() -> iodata().
erase_cursor_begin_line() ->
  esc([$[, $1, $K]).

erase_cursor_line_begin_test() ->
  ?assertEqual([27, $[, $1, $K], lists:flatten(erase_cursor_begin_line())).

-spec erase_line() -> iodata().
erase_line() ->
  esc([$[, $2, $K]).

erase_line_test() ->
  ?assertEqual([27, $[, $2, $K], lists:flatten(erase_line())).
