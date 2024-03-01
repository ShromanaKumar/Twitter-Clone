-module(twitter_server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1, user/1]).
-export([send_tweets/5]).


start(_Type, _Args) ->
	State = [dict:new(), dict:new()],
	Pid = spawn(twitter_server_app, user, [State]),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/ws", twitter_handler, [Pid]}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
			env => #{dispatch => Dispatch}
		}),
    twitter_server_sup:start_link().

stop(_State) ->
	ok.

user(State) ->
	receive
		{register_user, UserID, Pid} ->
			UserPid = lists:nth(1, State),
			PidUsers = lists:nth(2, State),
			New_User_Pid = dict:store(UserID, Pid, UserPid),
			New_Pid_User = dict:store(Pid, UserID, PidUsers),
			user([New_User_Pid, New_Pid_User]);
		{disconnect_user, Pid} ->
			UserPid = lists:nth(1, State),
			PidUsers = lists:nth(2, State),
			UserID = dict:find(Pid, PidUsers),
			New_User_Pid = dict:erase(UserID, UserPid),
			New_Pid_User = dict:erase(Pid, PidUsers),
			user([New_User_Pid, New_Pid_User]);
		{send_tweets, Tweet, EndUser, UserID, TweetID} ->
			spawn(twitter_server_app, send_tweets, [Tweet, EndUser, lists:nth(1, State), UserID, TweetID]),
			user(State)
	end.

send_tweets(_, [], _,_,_) ->
	ok;
send_tweets(Tweet, EndUser, LogIn, UserID, TweetId) ->
	[User | Remainder] = EndUser,
	UserString = binary_to_list(lists:nth(1, User)),
	case dict:find(UserString, LogIn) of
		{ok, C_Pid} ->
			C_Pid ! {new_tweet, Tweet, UserID, TweetId};
		error ->
			ok
	end,
	send_tweets(Tweet, Remainder, LogIn, UserID, TweetId).