-module(twitter_handler).
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).
-export([get_PID/2, registerUser/2, login/2, getPassword/1]).
-export([displayTweet/0,getUserName/0,getSubscription/1, getFinalTweetList/2,display/1]).
-export([addTweet/1,getUserList/0,getAllTweets/2,displayWithHashtag/2,searchWithHashtag/1]).
-export([addSubscription/1, reTweetHelper/1, reTweet/1]).


init(Req, State) ->
    io:format("websocket connection initiated~n~p~n~nstate: ~p~n", [Req, State]),
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    {ok, State}.


websocket_handle({text,_Data}, State) ->
    DecodingData = jsone:decode(_Data),
    QueryType = binary_to_list(maps:get(<<"Query">>, DecodingData)),
    UserID = binary_to_list(maps:get(<<"UserID">>, DecodingData)),
    case QueryType of
        "login" ->
            Password = binary_to_list(maps:get(<<"Password">>, DecodingData)),
            Status = login(UserID, Password),
            Msg = #{<<"response">> => Status, 
                <<"UserID">> => list_to_binary(UserID)},
            EncodingMsg = jsone:encode(Msg),
            CurrentPid = self(),
            [User_Handler] = State,
            User_Handler ! {register_user, UserID, CurrentPid},
            {[{text, EncodingMsg}], State++[CurrentPid]};

        "register" ->
            Password = binary_to_list(maps:get(<<"Password">>, DecodingData)),
            Status = registerUser(UserID, Password),
            Msg = #{<<"response">> => Status, 
                <<"UserID">> => list_to_binary(UserID)},
            EncodingMsg = jsone:encode(Msg),
            CurrentPid = self(),
            [User_Handler] = State,
            User_Handler ! {register_user, UserID, CurrentPid},
            {[{text, EncodingMsg}], State++[CurrentPid]};

        "addTweet" ->
            NewTweet = binary_to_list(maps:get(<<"Tweet">>, DecodingData)),
            {Status, EndUser, TweetID} =addTweet(NewTweet),
            Msg = #{<<"response">> => <<"T_response">>, <<"status">> => Status},
            EncodingMsg = jsone:encode(Msg),
            case Status of 
                <<"ok">> ->
                    [User_Handler, _] = State,
                    User_Handler ! {add_tweets, NewTweet, EndUser, UserID, TweetID};
                <<"error">> ->
                    ok
            end,
            {[{text,EncodingMsg}, State]};
        
        "search" ->
            HashorAt = binary_to_list(maps:get(<<"SearchString">>, DecodingData)),
            S_tweet = searchWithHashtag(HashorAt),
            {server_search, _, T_string, TweetID, NameString} = S_tweet,

            Msg = #{<<"response">> => <<"search_response">>, 
                <<"tweet">> => list_to_binary(T_string), <<"tweet_ID">> => TweetID,
                <<"tweeter">> => list_to_binary(NameString)},
            EncodingMsg = jsone:encode(Msg),
            {[{text, EncodingMsg}, State]};

        "subscribe" ->
            Subscription = binary_to_list(maps:get(<<"Subscibe_text">>, DecodingData)),
            addSubscription(Subscription),
            {ok, State};
        "retweet" ->
            TweetID = maps:get(<<"TweetID">>, DecodingData),
            {Status, EndUser, NewID, Tweet} = reTweet(TweetID),
            Msg = #{<<"response">> => <<"T_response">>, 
                <<"status">> => Status},
            EncodingMsg = jsone:encode(Msg),
            case Status of 
                <<"ok">> ->
                    [User_Handler, _] = State,
                    User_Handler ! {add_tweets, Tweet, EndUser, UserID, NewID};
                <<"error">> ->
                    ok
            end,
            {[{text,EncodingMsg}, State]}
    end;

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({new_tweet, Tweet, Twitter, TweetID}, State) ->
    Msg = #{<<"response">> => <<"new_tweet">>, 
        <<"tweet">> => list_to_binary(Tweet), <<"tweet_ID">> => TweetID, 
        <<"twitter">> => list_to_binary(Twitter)},
    EncodingMsg = jsone:encode(Msg),
    {[{text,EncodingMsg}, State]};

websocket_info(_Info, State) ->
    {ok, State}.

%%%%%%Twitter Functions%%%%%%

getPassword(UserName) ->
	if UserName /= "" ->
		get_pid ! [get_pid, self(), UserName];
	true ->
		io:format("")
	end,

	receive
        [here_pid, User_PID] ->
			User_PID ! [get_password, self()],
			getPassword("");
		[here_password, Password] ->
			Password
	end.

addTweet(Tweet) ->
	UserName = getUserName(),
	TweetList = getTweet(UserName),
	DisplayTime = erlang:universaltime(),
	NonDisplayTime = os:system_time(),
	CurrTweet = {NonDisplayTime, UserName, Tweet, DisplayTime},
	New_Tweets = TweetList ++ [CurrTweet],
	get_pid ! [get_pid, self(), UserName],

	receive
        [here_pid, User_PID] ->
			User_PID ! [add_tweets, New_Tweets],
			io:format("Tweets added.~n")
	end.


login(UserName, Password) ->
	%{StartingWCTime, _} = statistics(wall_clock),
	ActualPassword = getPassword(UserName),
	if ActualPassword == Password ->
		store_username ! [add_username, UserName],
		io:format("Login Successful ~n"),
		displayTweet();
	true ->
		io:format("Login Unsuccessful ~n")
	end.
	%{EndingWCTime, _} = statistics(wall_clock),
    %io:format("Total convergence took ~w seconds ~n", [(EndingWCTime - StartingWCTime)/1000]).



getUserName() ->
	store_username ! [get_username, self()],
	receive
		[here_username, UserName] ->
			UserName
	end.



displayTweet() ->
	UserName = getUserName(),
	Subs = getSubscription(UserName),
	TotalUserList = Subs ++ [UserName],
	TweetList = getFinalTweetList(TotalUserList, []),
	SortedTweetList = lists:sort(TweetList),
	io:format("Here is your daily dose of tweets.~n~n"), 
	display(SortedTweetList).

display([]) -> io:format("");
display(SortedTweetList) -> 
	TweetList = lists:last(SortedTweetList),
	{_, UserName, Tweet, {{Year, Month, Day}, {Hr, Min, _}}} = TweetList,
	io:format("@~s: ~s ~nDate: ~w~w~w Time ~w:~w UTC.~n~n", [UserName, Tweet, Year, Month, Day, Hr, Min]), 
	NewSortedTweetList = lists:droplast(SortedTweetList),
	display(NewSortedTweetList).

getFinalTweetList([User | []], TweetList) ->
	Tweet = getTweet(User),
	New_TweetList = TweetList ++ Tweet,
	New_TweetList;

getFinalTweetList([User | Rest], TweetList) ->
	Tweet = getTweet(User),
	New_TweetList = TweetList ++ Tweet,
	getFinalTweetList(Rest, New_TweetList).

getSubscription(UserName) ->
	if UserName /= "" ->
		get_pid ! [get_pid, self(), UserName];
	true ->
		io:format("")
	end,

	receive
        [here_pid, User_PID] ->
			User_PID ! [get_subs, self()],
			getSubscription("");
		[here_subs, Subs] ->
			Subs
	end.

getTweet(UserName) ->
	if UserName /= "" ->
		get_pid ! [get_pid, self(), UserName];
	true ->
		io:format("")
	end,

	receive
        [here_pid, User_PID] ->
			User_PID ! [get_tweets, self()],
			getTweet("");
		[here_tweets, Tweets] ->
			Tweets
	end.


registerUser(UserName, Password) ->
	PID = spawn(twitter_client, user, [UserName, Password, [], []]),
	get_pid ! [add_pid, UserName, PID],
	ok.

index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> [];
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

get_PID(UserNameList, PIDList) ->
	receive
        [add_pid, UserName, PID] ->
			New_PIDList = PIDList ++ [PID],
			New_UserNameList = UserNameList ++ [UserName],
			get_PID(New_UserNameList, New_PIDList);
		[get_pid, PID, UserName] ->
			Index = index_of(UserName, UserNameList),
			User_PID = lists:nth(Index, PIDList),
			PID ! [here_pid, User_PID];
		[get_userlist, PID] ->
			PID ! [here_userlist, UserNameList]	
    end,
	get_PID(UserNameList, PIDList).

searchWithHashtag(Hashtag) ->
	UserNameList = getUserList(),
	TweetList = getAllTweets(UserNameList, []),
	SortedTweetList = lists:sort(TweetList),
	displayWithHashtag(SortedTweetList, Hashtag).

getUserList() ->
	get_pid ! [get_userlist, self()],
	receive
        [here_userlist, UserNameList] ->
			UserNameList
	end.

getAllTweets([], TweetList) ->
	TweetList;

getAllTweets([User1 | []], TweetList) ->
	Tweet = getTweet(User1),
	New_TweetList = TweetList ++ Tweet,
	New_TweetList;

getAllTweets([User1 | Rest], TweetList) ->
	Tweet = getTweet(User1),
	New_TweetList = TweetList ++ Tweet,
	getAllTweets(Rest, New_TweetList).

displayWithHashtag([], _) -> io:format("");

displayWithHashtag(SortedTweetList, Hashtag) ->
	TweetList = lists:last(SortedTweetList),
	{_, UserName, Tweet, {{Year, Month, Day}, {Hr, Min, _}}} = TweetList,
	Match = re:run(Tweet, Hashtag),
	if Match == nomatch ->
		io:format("");
	true ->
		io:format("@~s: ~s ~nDate: ~w~w~w Time ~w:~w UTC.~n~n", [UserName, Tweet, Year, Month, Day, Hr, Min])
	end,
	NewSortedTweetList = lists:droplast(SortedTweetList),
	displayWithHashtag(NewSortedTweetList, Hashtag).

addSubscription(SubsName) ->
	UserName = getUserName(),
	Subs = getSubscription(UserName),
	New_Subs = Subs ++ [SubsName],
	get_pid ! [get_pid, self(), UserName],

	receive
        [here_pid, User_PID] ->
			User_PID ! [add_subs, New_Subs],
			io:format("Subscription added.~n")
	end.

reTweetHelper(TargetTweet) ->
	TotalNum = length(TargetTweet),
	Selected = rand:uniform(TotalNum),
	SelectedTweet = lists:nth(Selected, TargetTweet),
	{_, UserName, Tweet, {{_, _, _}, {_, _, _}}} = SelectedTweet,
	RT_Tweet = "RT : @"++UserName++" " ++Tweet,
	RT_Tweet.

reTweet(UserName) ->
	CurrUser = getUserName(),
	CurrTweet = getTweet(CurrUser),

	TargetTweet = getTweet(UserName),
	RT_Tweet = reTweetHelper(TargetTweet),

	DisplayTime = erlang:universaltime(),
	NonDisplayTime = os:system_time(),
	ReTweet = {NonDisplayTime, CurrUser, RT_Tweet, DisplayTime},

	New_Tweets = CurrTweet ++ [ReTweet],
	get_pid! [get_pid, self(), CurrUser],

	receive
        [here_pid, User_PID] ->
			User_PID ! [add_tweets, New_Tweets],
			io:format("ReTweets added.~n")
	end.
	

terminate(_Reason, Req, _State) ->
    io:format("websocket connection terminated~n~p~n", [maps:get(peer, Req)]),
    ok.