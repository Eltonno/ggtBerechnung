-module(ggt_process).
-export([start/9, init_ggt/1, term_request/1]).

start(ArbeitsZeit,TermZeit,Quota,Praktikumsgruppe,Teamnummer,GGTProzessnummer,Starternummer,NameS,Koordinator) ->
  {ok,NodeName} = inet:gethostname(),
  %Der Name des ggt-Prozesses ist eine Zahlenfolge die folgendermaßen erstellt wird: <PraktikumsgruppenID><TeamID><Nummer des ggT-Prozess><Nummer des Starters>
  GGTName = list_to_atom(integer_to_list(Praktikumsgruppe) ++ integer_to_list(Teamnummer) ++ integer_to_list(GGTProzessnummer) ++ integer_to_list(Starternummer)),

  %Der ggt-Prozess hat eine Liste von Tupeln ‘Config’, die folgende Daten speichert: – Einstellungen aus der Config.
  Config = [{ggtname, GGTName},
    {mi, undefined},   			              %– Aktuelle Mi-Zahl
  	{leftn, undefined},   		            %– Die Nachbarn
  	{rightn, undefined},   		            %– Die Nachbarn
  	{arbeitszeit, ArbeitsZeit},
    {termzeit, TermZeit*1000},
    {quota, Quota},
    {names, NameS},
    {koordinator, Koordinator},
    {letztenachricht, vsutil:getUTC()},   %– Timestamp der letzten Änderung von Mi
  	{tref, undefined},
    {votes, 0},                           %– Anzahl der Empfangenen yesVotes.
    {logfile, "GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ NodeName ++ ".log"}],
  spawn(?MODULE, init_ggt, [Config]).

init_ggt(Config) ->
  {names, NameS} = keyfind(names, Config),
  {ggtname, GGTName} = keyfind(ggtname, Config),
  {logfile, Logfile} = keyfind(logfile, Config),
  {koordinator, Koordinator} = keyfind(koordinator, Config),
  util:logging(Logfile, util:to_String(GGTName) ++ "\n"),

  %Er registriert sich ebenfalls lokal auf der Erlang-Node mit seinem Namen (register)
  register(GGTName, self()),
  %und beim Namensdienst (rebind)
  NameS ! {self(),{rebind,GGTName,node()}},
  receive
    ok ->
      util:logging(Logfile, util:to_String(GGTName) ++ " hat sich am Namensdienst und Lokal registriert\n")
  end,
  %Der ggT-Prozess meldet sich beim Koordinator mit seinem Namen an (hello)
  Koordinator ! {hello, GGTName},
  util:logging(Logfile, util:to_String(GGTName) ++ " hat sich am Koordinator angemeldet\n"),
  start_ggt(Config).

start_ggt(Config) ->
  {names, NameS} = keyfind(names, Config),
  {ggtname, GGTName} = keyfind(ggtname, Config),
  {logfile, Logfile} = keyfind(logfile, Config),
  util:logging(Logfile, util:to_String(GGTName) ++ "\n"),
  receive
    {setneighbors,LeftN,RightN} ->
      util:logging(Logfile, util:to_String(GGTName) ++ " setneighbors -> LeftN: " ++ atom_to_list(LeftN) ++ " setneighbors -> RightN: " ++ atom_to_list(RightN) ++ "\n"),
      %Setzt die Nachbarn in Liste ‘Config’
      LConfig = keystore(leftn, Config, {leftn, LeftN}),
      RLConfig = keystore(rightn, LConfig, {rightn, RightN}),
      arbeitsphase_ggt(RLConfig);
    kill ->
      %Loggt, dass der GGT gekillt wird.
      util:logging(Logfile, atom_to_list(GGTName) ++ " wird gekilled\n"),
      %Unbindet vom Namensdienst via {From, {unbind, GGTName}}
      NameS ! {self(),{unbind, GGTName}},
      %Terminiert sich selbst.
      unregister(GGTName)
  end
.

arbeitsphase_ggt(Config) ->
  {mi, Mi} = keyfind(mi, Config),
  {koordinator, Koordinator} = keyfind(koordinator, Config),
  {ggtname, GGTName} = keyfind(ggtname, Config),
  {arbeitszeit, ArbeitsZeit} = keyfind(arbeitszeit, Config),
  {leftn, LeftN} = keyfind(leftn, Config),
  {rightn, RightN} = keyfind(rightn, Config),
  {termzeit, TermZeit} = keyfind(termzeit, Config),
  {tref, TRef} = keyfind(tref, Config),
  {names, NameS} = keyfind(names, Config),
  {logfile, Logfile} = keyfind(logfile, Config),
  {votes, Votes} = keyfind(votes, Config),
  {quota, Quota} = keyfind(quota, Config),
  {letztenachricht, LetzteNachricht} = keyfind(letztenachricht, Config),
  {names, NameService} = keyfind(names, Config),
  receive
    %Vor einer ggT-Berechnung erwartet der ggT-Prozess vom Koordinator seine Zahl Mi (setpm).
    {setpm, MiNeu} ->
      timer:cancel(TRef),
      %Mi wird in ‘Config’ aktualisiert.
      MConfig = keystore(mi, Config, {mi, MiNeu}),
      {ok, CTRef} = timer:apply_after(TermZeit,?MODULE,term_request, [MConfig]),
      TMConfig = keystore(tref, MConfig, {tref, CTRef}),
      LastTMConfig = keystore(votes, keystore(letztenachricht, TMConfig, {letztenachricht, vsutil:getUTC()}), {votes, 0}),
      arbeitsphase_ggt(LastTMConfig);
    {sendy, Y} ->
      timer:cancel(TRef),
      util:logging(Logfile, util:to_String(GGTName) ++ " " ++ integer_to_list(Y) ++ "\n"),
      util:logging(Logfile, util:to_String(GGTName) ++ " LeftN: " ++ atom_to_list(LeftN) ++ "\n"),
      util:logging(Logfile, util:to_String(GGTName) ++ " RightN: " ++ atom_to_list(RightN) ++ "\n"),
      if
        %Euklid-Berechnung
      %Der rekursive Aufruf der ggT Berechnung.
      %Er startet nur die Berechnung, falls Y < Mi ist.
        Y < Mi ->
          util:logging(Logfile, atom_to_list(GGTName) ++ " macht Euklid-Berechnung " ++ "\n"),
          NewMi = ((Mi-1) rem Y) + 1,
          MConfig = keystore(mi, Config, {mi, NewMi}),
          NameService ! {self(),{lookup, LeftN}},
          receive
            not_found ->
              util:logging(Logfile, util:to_String(GGTName) ++ " LeftN not found\n");
            {pin, LeftNPID} ->
              LeftNPID ! {sendy, NewMi}
          end,
          NameService ! {self(),{lookup, RightN}},
          receive
            not_found ->
              util:logging(Logfile, util:to_String(GGTName) ++ " RightN not found\n");
            {pin, RightNPID} ->
              RightNPID ! {sendy, NewMi}
          end,
          %Bei jeder Berechnung tut der ggt für die Dauer der Arbeitszeit nichts.
          timer:sleep(ArbeitsZeit),

          %Wenn sich die Zahl des ggt durch eine Berechnung geändert hat, teilt er dies dem Koordinator mit (briefmi).
          Koordinator ! {briefmi,{GGTName,NewMi,util:timeMilliSecond()}},

          %Wenn der ggt seit <Termzeit> keine Zahl empfangen hat, sendet er eine Terminierungsanfrage.
          {ok, CTRef} = timer:apply_after(TermZeit,?MODULE,term_request, [MConfig]),
          TMConfig = keystore(tref, MConfig, {tref, CTRef}),
          LastTMConfig = keystore(letztenachricht, TMConfig, {letztenachricht, vsutil:getUTC()}),
          util:logging(Logfile,util:to_String(GGTName) ++ " hat sein Mi upgedatet " ++ util:to_String(vsutil:now2string(erlang:timestamp())) ++ " zu: " ++ integer_to_list(NewMi) ++ "\n"),
          arbeitsphase_ggt(keystore(tref, keystore(votes, LastTMConfig, {votes, 0}),{tref, CTRef}));
        true ->
	      {ok, CTRef} = timer:apply_after(TermZeit,?MODULE,term_request, [Config]),
          util:logging(Logfile, util:to_String(GGTName) ++ " Keine Änderung am Mi " ++ util:to_String(vsutil:now2string(erlang:timestamp())) ++ "\n"),
          LastConfig = keystore(letztenachricht, Config, {letztenachricht, vsutil:getUTC()}),
          arbeitsphase_ggt(keystore(tref, LastConfig, {tref, CTRef}))
      end;
    %Wenn der ggt eine Terminierungsanfrage empfängt, so antwortet er mit yes, falls er seit seiner halben Termzeit keine Zahl empfangen hat.
  % Ansonsten ignoriert er diese Terminierungsanfrage.
    {From,{vote,Initiator}} ->
      CTime = vsutil:getUTC(),
      Comp = (LetzteNachricht + (TermZeit / 2)) - CTime,
      if
        Initiator == GGTName ->
          arbeitsphase_ggt(Config);
        Comp =< 0 ->
          util:logging(Logfile, atom_to_list(Initiator) ++ " hat Terminierungswahl gestartet und " ++ atom_to_list(GGTName) ++ " stimmt YES " ++ util:to_String(vsutil:now2string(erlang:timestamp())) ++ "\n"),
          %NameS ! {self(),{lookup, Initiator}},
        	%	receive
        	%		not_found ->
          %     error;
        	%		{pin, Initiator} ->
          %      Initiator ! {voteYes, GGTName}
        	%	end,
          From ! {voteYes, GGTName},
          arbeitsphase_ggt(Config);
	      true ->
          util:logging(Logfile, atom_to_list(Initiator) ++ " hat Terminierungswahl gestartet und " ++ atom_to_list(GGTName) ++ " stimmt nicht ab " ++ util:to_String(vsutil:now2string(erlang:timestamp())) ++ "\n"),
          arbeitsphase_ggt(Config)
      end;
    {voteYes,Name} ->
      VConfig = keystore(votes, Config, {votes, Votes+1}),
      util:logging(Logfile, atom_to_list(Name) ++ " hat der Terminierung als " ++ integer_to_list(Votes+1) ++ ". zugestimmt\n"),
      % Wenn diese angenommen wurde, weil die Quote erreicht wurde, sendet er dem Koordinator eine Mitteilung über die Terminierung der aktuellen Berechnung,
      %die seinen Namen, den errechneten ggT (sein aktuelles Mi) und seine aktuelle Systemzeit beinhaltet.
      if
        (Votes + 1) == Quota ->
          util:logging(Logfile, "Quota wurde erreicht\n"),
          Koordinator ! {self(),briefterm,{GGTName,Mi,util:timeMilliSecond()}},
          arbeitsphase_ggt(VConfig);
        true ->
          arbeitsphase_ggt(VConfig)
      end;
    {From,tellmi} ->
      %Sendet das aktuelle Mi (aus ‘state’) an From (ist PID)
      From ! {mi, Mi},
      arbeitsphase_ggt(Config);
    {From,pingGGT} ->
      %Sendet ein pongGGT an From (ist PID)
      From ! {pongGGT, GGTName},
      arbeitsphase_ggt(Config);
    kill ->
      %Loggt, dass der GGT gekillt wird.
      util:logging(Logfile, atom_to_list(GGTName) ++ " wird gekilled\n"),

      %Unbindet vom Namensdienst via {From, {unbind, GGTName}}
      NameS ! {self(),{unbind, GGTName}},
      %Terminiert sich selbst.
      unregister(GGTName)
  end.

term_request(Config) ->
  {_, NameS} = keyfind(names, Config),
  {_, GGTName} = keyfind(ggtname, Config),
  NameS ! {self(),{multicast, vote, GGTName}},
  arbeitsphase_ggt(Config).

keystore(_,[],Tupel) ->
  [Tupel];
keystore(Key,[Head|Rest],Tupel) ->
  {K,_}= Head,
  if K == Key -> append(Tupel, Rest);
    true -> keystore(Key,Rest,[Head],Tupel)
  end.

keystore(_,[],Front,Tupel) ->
  append(Front, Tupel);
keystore(Key,[Head|Rest],Front,Tupel) ->
  {K,_} = Head,
  if K == Key -> append(append(Front,Tupel),Rest);
    true -> keystore(Key,Rest,append(Front,Head),Tupel)
  end.

keyfind(_,[]) ->
  false;
keyfind(Key, Tuplelist) ->
  [Head|Rest] = Tuplelist,
  {K,_} = Head,
  if K == Key -> Head;
    true -> keyfind(Key,Rest)
  end.

append([], []) ->
  [];
append([],[H|T]) ->
  [H|T];
append([],Elem) ->
  [Elem];
append([H|T], []) ->
  [H|T];
append(Elem, []) ->
  [Elem];
append([H1|T1],[H2|T2]) ->
  append([H1|T1] ++ [H2], T2);
append([H|T],Elem) ->
  [H|T] ++ [Elem];
append(L,[H|T]) ->
  append([L] ++ [H], T);
append(E1,E2) ->
  [E1] ++ [E2].