-module(twitter_client).

-export([register_user/2, login/2, start_listening/3, interface/1]).
-export([add_tweet/1, add_subscribe/1, add_retweet/1, add_search/1, startEngine/0]).

startEngine() ->
    {ok, _} = application:ensure_all_started(gun),
    {ok, ConnPid} = gun:open("localhost", 8080),
    {ok, _Protocol} = gun:await_up(ConnPid),
    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    timer:apply_interval(5000, gun, ws_send, [ConnPid, StreamRef, ping]),
    LoginPage = string:chomp(io:get_line("1. Login\n2. Register NewUser\n")),
    if LoginPage == "1" ->
        io:fwrite(" "),
        login(ConnPid, StreamRef);
    true ->
        io:fwrite(" "),
        register_user(ConnPid, StreamRef)
    end.


login(ConnPid, StreamRef) ->
    UserID = list_to_binary(string:chomp(io:get_line("Enter_ID\n"))),
    Password = list_to_binary(string:chomp(io:get_line("Enter_Password\n"))),
    %Message to be sent
    Message = #{<<"Query">> => "login", <<"UserID">> => UserID, <<"Password">> => Password},
    EncodingMsg = jsone:encode(Message),
    gun:ws_send(ConnPid, StreamRef, {text, EncodingMsg}),
    receive
        {gun_ws, _ConnPid, _StreamRef, {text, _Frame}} ->
            DecodingData = jsone:decode(_Frame),
            LoginRes = binary_to_list(maps:get(<<"response">>, DecodingData)),
            case LoginRes of
                "ok" ->
                    Rt_UserID = maps:get(<<"userID">>, DecodingData),
                    spawn(twitter_client, interface, [self()]),
                    start_listening(Rt_UserID, ConnPid, StreamRef);
                "error" ->
                    io:fwrite("Wrong ID"),
                    login(ConnPid, StreamRef)
            end
    end.

register_user(ConnPid, StreamRef) ->
    UserID = list_to_binary(string:chomp(io:get_line("Enter_ID\n"))),
    Password = list_to_binary(string:chomp(io:get_line("Enter_Password\n"))),
    %Message to be sent
    Message = #{<<"Query">> => "register", <<"UserID">> => UserID, <<"Password">> => Password},
    EncodingMsg = jsone:encode(Message),
    gun:ws_send(ConnPid, StreamRef, {text, EncodingMsg}),
    receive
        {gun_ws, _ConnPid, _StreamRef, {text, _Frame}} ->
            DecodingData = jsone:decode(_Frame), 
            Register_ans = binary_to_list(maps:get(<<"response">>, DecodingData)),
            case Register_ans of
                "ok" ->
                    Rt_UserID = maps:get(<<"userID">>, DecodingData),
                    spawn(twitter_client, interface, [self()]),
                    start_listening(Rt_UserID, ConnPid, StreamRef);
                "error" ->
                    io:fwrite("Error_registering"),
                    login(ConnPid, StreamRef)
            end
    end.

start_listening(UserID, ConnPID, StreamRef) ->
	receive
		{gun_ws, _ConnPID, _StreamRef, {text,_Frame}} ->
			DecodingData = jsone:decode(_Frame),
			Responsing_Type = binary_to_list(maps:get(<<"response">>)),
			case Responsing_Type of 
				"T_response" ->
					Status = binary_to_list(maps:get(<<"status">>, DecodingData)),
					case Status of
						"error" ->
							io:fwrite("This is incorrect Tweet\n");
						"ok" ->
							io:fwrite("Tweet success!\n")
					end;
				"search_response" ->
					RetTweet = binary_to_list(maps:get(<<"tweet">>, DecodingData)),
					RetTweetID = (maps:get(<<"tweet_ID">>, DecodingData)),
					RetTweeter = binary_to_list(maps:get(<<"tweeter">>, DecodingData)),
					io:fwrite("Tweet ~p \t, Tweet ID ~p \t, Tweeter ~p \n ",[RetTweet, RetTweetID, RetTweeter]);
				"new_tweet" ->
					NewTweet = binary_to_list(maps:get(<<"tweet">>, DecodingData)),
					NewTweetID = (maps:get(<<"tweet_ID">>, DecodingData)),
					NewTweeter = binary_to_list(maps:get(<<"twitter">>, DecodingData)),
					io:fwrite("Tweet ~p \t, Tweet ID ~p \t, Tweeter ~p \n ",[NewTweet, NewTweetID, NewTweeter])
			end;

		{client_action, Action, Data} ->
			case Action of
				"addTweet" ->
					Msg = #{<<"Query">> => <<"addTweet">>, <<"UserID">> => UserID, 
							<<"Tweet">> => Data},
					EncodingMsg = jsone:encode(Msg),
					gun:ws_send(ConnPID, StreamRef, {text, EncodingMsg});
				"subscribe" ->
					Msg = #{<<"Query">> => <<"subscribe">>, <<"UserID">> => UserID, 
							<<"Subscribe_text">> => Data},
					EncodingMsg = jsone:encode(Msg),
					gun:ws_send(ConnPID, StreamRef, {text, EncodingMsg});
				"retweet" ->
					Msg = #{<<"Query">> => <<"retweet">>, <<"UserID">> => UserID, 
							<<"TweetID">> => Data},
					EncodingMsg = jsone:encode(Msg),
					gun:ws_send(ConnPID, StreamRef, {text, EncodingMsg});
				"search" ->
					Msg = #{<<"Query">> => <<"search">>, <<"UserID">> => UserID, 
							<<"SearchString">> => Data},
					EncodingMsg = jsone:encode(Msg),
					gun:ws_send(ConnPID, StreamRef, {text, EncodingMsg})
			end
	end,
	start_listening(UserID, ConnPID, StreamRef).



interface(Pid) ->
	Action = string:chomp(io:get_line ("1. Tweet \n, 2. Subscribe \n, 3. Retweet \n, 4. Search")),
	case Action of
		"1" -> add_tweet(Pid);
		"2" -> add_subscribe(Pid);
		"3" -> add_retweet(Pid);
		"4" -> add_search(Pid)
	end.

add_tweet(Pid) ->
	{ok, Tweet} = io:read(),
	Pid ! {client_action, "addTweet", Tweet}.

add_subscribe(Pid) ->
	{ok, Subscribe_string} = io:read(),
	Pid ! {client_action, "subscribe", Subscribe_string}.

add_retweet(Pid) ->
	{ok, TweetID} = io:read(),
	Pid ! {client_action, "retweet", TweetID}.

add_search(Pid) ->
	{ok, Search_string} = io:read(),
	Pid ! {client_action, "search", Search_string}.

				
