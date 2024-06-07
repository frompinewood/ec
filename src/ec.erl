-module(ec).

-export(['ESC'/0, 'ESC'/1, 'HOME'/0, 'MV'/2, 'RIGHT'/1, 'UP'/1, 'LEFT'/1, 'DOWN'/1,
         'NEXT'/1, 'PREV'/1, 'COL'/1]).
-export(['ERASE_IN_DISPLAY'/0, 'ERASE_CURSOR_END_SCREEN'/0, 'ERASE_CURSOR_BEGIN_SCREEN'/0,
         'ERASE_SCREEN'/0]).
-export(['ERASE_IN_LINE'/0, 'ERASE_CURSOR_END_LINE'/0, 'ERASE_CURSOR_BEGIN_LINE'/0,
         'ERASE_LINE'/0]).

-include_lib("eunit/include/eunit.hrl").

-spec 'ESC'() -> 16#1B.
'ESC'() ->
  16#1B.

'ESC'(Terms) ->
  ['ESC'(), Terms].

escape_test() ->
  ?assertEqual($\e, 'ESC'()).

%% Cursor commands
-spec 'HOME'() -> iodata().
'HOME'() ->
  ['ESC'(), $[, $H].

home_test() ->
  ?assertEqual([27, $[, $H], lists:flatten('HOME'())).

-spec 'MV'(non_neg_integer(), non_neg_integer()) -> iodata().
'MV'(X, Y) ->
  'ESC'([$[, integer_to_list(Y), $;, integer_to_list(X), $H]).

mv_test() ->
  ?assertEqual([27, $[, $2, $;, $1, $0, $H], lists:flatten('MV'(10, 2))).

-spec 'UP'(non_neg_integer()) -> iodata().
'UP'(A) ->
  'ESC'([$[, integer_to_list(A), $A]).

up_test() ->
  ?assertEqual([27, $[, $1, $0, $A], lists:flatten('UP'(10))).

-spec 'DOWN'(non_neg_integer()) -> iodata().
'DOWN'(A) ->
  'ESC'([$[, integer_to_list(A), $B]).

down_test() ->
  ?assertEqual([27, $[, $1, $0, $B], lists:flatten('DOWN'(10))).

-spec 'LEFT'(non_neg_integer()) -> iodata().
'LEFT'(A) ->
  'ESC'([$[, integer_to_list(A), $C]).

left_test() ->
  ?assertEqual([27, $[, $1, $0, $C], lists:flatten('LEFT'(10))).

-spec 'RIGHT'(non_neg_integer()) -> iodata().
'RIGHT'(A) ->
  'ESC'([$[, integer_to_list(A), $D]).

right_test() ->
  ?assertEqual([27, $[, $1, $0, $D], lists:flatten('RIGHT'(10))).

-spec 'NEXT'(non_neg_integer()) -> iodata().
'NEXT'(A) ->
  'ESC'([$[, integer_to_list(A), $E]).

next_test() ->
  ?assertEqual([27, $[, $1, $0, $E], lists:flatten('NEXT'(10))).

-spec 'PREV'(non_neg_integer()) -> iodata().
'PREV'(A) ->
  'ESC'([$[, integer_to_list(A), $F]).

prev_test() ->
  ?assertEqual([27, $[, $1, $0, $F], lists:flatten('PREV'(10))).

-spec 'COL'(non_neg_integer()) -> iodata().
'COL'(A) ->
  'ESC'([$[, integer_to_list(A), $G]).

col_test() ->
  ?assertEqual([27, $[, $1, $0, $G], lists:flatten('COL'(10))).

%% Erase
-spec 'ERASE_IN_DISPLAY'() -> iodata().
'ERASE_IN_DISPLAY'() ->
  'ESC'([$[, $J]).

erase_in_display_test() ->
  ?assertEqual([27, $[, $J], lists:flatten('ERASE_IN_DISPLAY'())).

-spec 'ERASE_CURSOR_END_SCREEN'() -> iodata().
'ERASE_CURSOR_END_SCREEN'() ->
  'ESC'([$[, $0, $J]).

erase_cursor_end_test() ->
  ?assertEqual([27, $[, $0, $J], lists:flatten('ERASE_CURSOR_END_SCREEN'())).

-spec 'ERASE_CURSOR_BEGIN_SCREEN'() -> iodata().
'ERASE_CURSOR_BEGIN_SCREEN'() ->
  'ESC'([$[, $1, $J]).

erase_cursor_begin_test() ->
  ?assertEqual([27, $[, $1, $J], lists:flatten('ERASE_CURSOR_BEGIN_SCREEN'())).

-spec 'ERASE_SCREEN'() -> iodata().
'ERASE_SCREEN'() ->
  'ESC'([$[, $2, $J]).

erase_screen_test() ->
  ?assertEqual([27, $[, $2, $J], lists:flatten('ERASE_SCREEN'())).

-spec 'ERASE_IN_LINE'() -> iodata().
'ERASE_IN_LINE'() ->
  'ESC'([$[, $K]).

erase_in_line_test() ->
  ?assertEqual([27, $[, $K], lists:flatten('ERASE_IN_LINE'())).

-spec 'ERASE_CURSOR_END_LINE'() -> iodata().
'ERASE_CURSOR_END_LINE'() ->
  'ESC'([$[, $0, $K]).

erase_cursor_line_end_test() ->
  ?assertEqual([27, $[, $0, $K], lists:flatten('ERASE_CURSOR_END_LINE'())).

-spec 'ERASE_CURSOR_BEGIN_LINE'() -> iodata().
'ERASE_CURSOR_BEGIN_LINE'() ->
  'ESC'([$[, $1, $K]).

erase_cursor_line_begin_test() ->
  ?assertEqual([27, $[, $1, $K], lists:flatten('ERASE_CURSOR_BEGIN_LINE'())).

-spec 'ERASE_LINE'() -> iodata().
'ERASE_LINE'() ->
  'ESC'([$[, $2, $K]).

erase_line_test() ->
  ?assertEqual([27, $[, $2, $K], lists:flatten('ERASE_LINE'())).
