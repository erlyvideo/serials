-ifndef(TEST).
-define(D(Term), lager:info("~240p", [Term])).
-define(D(Fmt, Args), lager:info(Fmt, Args)).
-else.
-define(D(Term), io:format(user, "~s:~w ~240p\n", [?FILE, ?LINE, Term])).
-define(D(Fmt, Args), io:format(user, "~s:~w "++Fmt++"\n", [?FILE, ?LINE | Args])).
-endif.

-define(VIDEO_EXT, [".avi", ".mp4", ".mkv", ".mov", ".mpg", ".wmv"]).
