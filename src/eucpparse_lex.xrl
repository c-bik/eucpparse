Definitions.

Rules.

(\x02)    : {token, {'STX', TokenLine}}.
(\x03)    : {token, {'ETX', TokenLine}}.
(\x2F)    : {token, {'/',   TokenLine}}.
(\x2C)    : {token, {',',   TokenLine}}.
(O)       : {token, {'O',   TokenLine}}.
(R)       : {token, {'R',   TokenLine}}.
[A-F0-9]* : {token, {'DAT', TokenLine, TokenChars}}.

Erlang code.
%% Copyright (C) Bikram Chatterjee"
%% @private"
%% @Author Bikram Chatterjee"
%% @Email razorpeak@gmail.com".
