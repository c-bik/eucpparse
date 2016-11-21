Definitions.

Rules.

(\x02)    : {token, {'STX', TokenLine}}.
(\x03)    : {token, {'ETX', TokenLine}}.
(\x2F)    : {token, {'/',   TokenLine}}.
(\x2C)    : {token, {',',   TokenLine}}.

(O/60)    : {token, {'O60', TokenLine}}.

(R/60)    : {token, {'R60', TokenLine}}.

[A-F0-9]* : {token, {'DAT', TokenLine, TokenChars}}.

Erlang code.
%% Copyright (C) Bikram Chatterjee"
%% @private"
%% @Author Bikram Chatterjee"
%% @Email razorpeak@gmail.com".
