-module(starter).
-export([start/1]).

-define(NODE_NAME, node()).
-define(LOGFILE, integer_to_list(StarterID) ++ util:to_String(?NODE_NAME) ++ ".log").

%Startet den Starter mit einer eindeutigen ID.
start(StarterID) ->
  util:logging(?LOGFILE , "Starter gestartet\n"),

  %Die restlichen Einstellungen werden aus der ggt.cfg-Datei ausgelesen.
  {ok, Config} = file:consult("ggt.cfg"),
  {ok, Koordinatorname} = vsutil:get_config_value(koordinatorname, Config),
  {ok, NameServiceNode} = vsutil:get_config_value(nameservicenode, Config),
  {ok, Praktikumsgruppe} = vsutil:get_config_value(praktikumsgruppe, Config),
  {ok, Teamnummer} = vsutil:get_config_value(teamnummer, Config),

  %Ping den Nameservice an
  pong = net_adm:ping(NameServiceNode),
  timer:sleep(1000),
  NameService = global:whereis_name(nameservice),
  NameService ! {self(),{lookup, Koordinatorname}},
  receive
    not_found ->
      util:logging(?LOGFILE , "Koordinator nicht auf Nameservice vorhanden");
    {pin, {Name, Node}} ->
      pong = net_adm:ping(Node),
      KoordinatorNameNode = {Name, Node},
      {Name, Node} ! log,
      util:logging(?LOGFILE, util:to_String(KoordinatorNameNode) ++ "\n"),
      PID = self(),
      util:logging(?LOGFILE, util:to_String(PID) ++ "\n"),
      % Der Starter bekommt einige Einstellungen vom Koordinator durch “getsteeringval”.
      KoordinatorNameNode ! {PID, getsteeringval},
      receive
      %Empfängt die Einstellungen und startet daraufhin die ggt-Prozesse via eigener ggt-Methode “ggt-starter”.
        {steeringval,ArbeitsZeit,TermZeit,Quote,GGTProzessnummer} ->
          util:logging(?LOGFILE , "Hat steeringval bekommen"),
          State = [{arbeitszeit, ArbeitsZeit},
            {termzeit, TermZeit},
            {ggtprozessnummer, GGTProzessnummer},
            {koordinatorname, Koordinatorname},
            {quote, Quote},
            {koordinatornamenode, KoordinatorNameNode},
            {nameservice, NameService},
            {praktikumsgruppe, Praktikumsgruppe},
            {starterid, StarterID},
            {teamnummer, Teamnummer},
            {logfile, ?LOGFILE}],
          ggt_starter(1,State),
          exit(self(), normal);
        not_found ->
          util:logging(?LOGFILE, "Steeringval nicht gefunden")
      end
  end.


ggt_starter(Iterator, Config) ->
  {_, ArbeitsZeit} = keyfind(arbeitszeit, Config),
  {_, TermZeit} = keyfind(termzeit, Config),
  {_, GGTProzessnummer} = keyfind(ggtprozessnummer, Config),
  {_, KoordinatorNameNode} = keyfind(koordinatornamenode, Config),
  {_, Quote} = keyfind(quote, Config),
  {_, NameService} = keyfind(nameservice, Config),
  {_, Praktikumsgruppe} = keyfind(praktikumsgruppe, Config),
  {_, Teamnummer} = keyfind(teamnummer, Config),
  {_, StarterID} = keyfind(starterid, Config),
  spawn(ggt_process, start, [ArbeitsZeit*1000,TermZeit,Quote,Praktikumsgruppe,Teamnummer,Iterator,StarterID,NameService,KoordinatorNameNode]),
  if
    Iterator == GGTProzessnummer ->
      ok;
    true ->
      ggt_starter(Iterator+1,Config)
  end.

keyfind(_,[]) ->
  false;
keyfind(Key, Tuplelist) ->
  [Head|Rest] = Tuplelist,
  {K,_} = Head,
  if K == Key -> Head;
    true -> keyfind(Key,Rest)
  end.