-module(koordinator).
-export([start/0, start_ko/1, msg/1]).

start () ->
  start("koordinator.cfg").
start(File) ->
  spawn(?MODULE, start_ko, [File]).

start_ko(Datei) ->
  {ok, Hostname} = inet:gethostname(),
  Logfile = "Koordinator@" ++ Hostname ++ ".log",
  util:logging(Logfile, "Koordinator-" ++ util:to_String(node()) ++ "-LAW Startzeit: " ++ vsutil:now2string(erlang:timestamp()) ++ " mit PID " ++ util:to_String(self()) ++ "\n"),

  util:logging(Logfile, "koordinator.cfg geoffnet...\n"),
  %Die Einstellungen für den Koordinator werden aus der koordinator.cfg-Datei ausgelesen.
  %Diese beinhaltet:
  {ok, File} = file:consult(Datei),
  %Verzögerungszeit für die Berechnung (arbeitszeit)
  {ok, ArbeitsZeit} = vsutil:get_config_value(arbeitszeit, File),
  %Lebenszeit (termzeit)
  {ok, TermZeit} = vsutil:get_config_value(termzeit, File),
  %Anzahl der zu startenden GGT-Prozesse (ggtprozessnummer)
  {ok, GGTProzessnummer} = vsutil:get_config_value(ggtprozessnummer, File),
  %Namensdienst Node (nameservicenode)
  {ok, NameServiceNode} = vsutil:get_config_value(nameservicenode, File),
  %Name des Koordinators (koordinatorname)
  {ok, Koordinatorname} = vsutil:get_config_value(koordinatorname, File),
  %Für die Abstimmung benötigte Quote in % (quote)
  {ok, Quote} = vsutil:get_config_value(quote, File),
  %Ob der Koordinator bei Terminierungsmeldungen korrigierend eingreifen soll (korrigieren)
  {ok, Korrigieren} = vsutil:get_config_value(korrigieren, File),
  util:logging(Logfile, "koordinator.cfg gelesen...\n"),
  Config = [{arbeitszeit, ArbeitsZeit},
    {termzeit, TermZeit},
    {ggtprozessnummer, GGTProzessnummer},
    {nameservicenode, NameServiceNode},
    {koordinatorname, Koordinatorname},
    {quote, Quote},
    {korrigieren, Korrigieren},
    %Der Koordinator hat eine Liste der bei ihm registrierten ggt-Prozesse,
    {ggts, []},
    {clients, 0},
    %und eine Variable, in der die kleinste von den ggt-Prozessen empfangene Zahl gespeichert wird.
    {minmi, undefined},
    {logfile, Logfile}],
  {_, Koordinatorname} = keyfind(koordinatorname, Config),
  {_, NameServiceNode} = keyfind(nameservicenode, Config),
  util:logging(Logfile, "Nameservice nameservice auf " ++ util:to_String(NameServiceNode) ++ " gebunden...\n"),
  register(Koordinatorname, self()),
  util:logging(Logfile, "lokal mit euklid registriert...\n"),
  pong = net_adm:ping(NameServiceNode),
  timer:sleep(1000),
  %%util:logging(Logfile, util:to_String(Pong)),
  NameService = global:whereis_name(nameservice),
  NameService ! {self() ,{rebind, Koordinatorname, node()}},
  receive
    ok ->
      util:logging(Logfile, "beim Namensdienst mit Node " ++ util:to_String(node()) ++ " registriert.\n"),
      initial(keystore(nameservice, Config, {nameservice, NameService}))
  end.

%initial
initial(Config) ->
  {_, Logfile} = keyfind(logfile, Config),
  receive
    %Hier sendet er dem Starter, wenn von dem Starter angefragt,
  % die ggtprozessnummer, arbeitszeit, termzeit, und ggt- prozessnummer * 0.<quote>
    {From, getsteeringval} ->
      {_,Cl} = keyfind(clients,Config),
      {_,GgtNr} = keyfind(ggtprozessnummer, Config),
      util:logging(Logfile, "getsteeringval: " ++ util:to_String(From) ++ " (" ++ util:to_String(Cl + GgtNr) ++ ")\n"),
      {_, ArbeitsZeit} = keyfind(arbeitszeit, Config),
      {_, TermZeit} = keyfind(termzeit, Config),
      {_, Quote} = keyfind(quote, Config),
      {_, GGTProzessnummer} = keyfind(ggtprozessnummer, Config),
      Config_Tmp_1 = keystore(clients, Config, {clients, Cl + GgtNr}),
      %Sendet From (Starter) seine Einstellungs-Werte.
      %Dies sind die Arbeitszeit, die Termzeit, die Quote, und die GGTProzessnummer.
      From ! {steeringval,ArbeitsZeit,TermZeit,trunc(GGTProzessnummer*(Quote/100)),GGTProzessnummer},
      util:logging(Logfile, util:to_String(From) ++ " die steeringval-Werte gesendet\n"),
      initial(Config_Tmp_1);
  %Registriert den ggt-Prozess.
    {hello, Clientname} ->
      {_, GGTs} = keyfind(ggts, Config),
      util:logging(Logfile, util:to_String(Clientname) ++ " sendet Hallo\n"),
      %Dazu fügt der Koordinator den ggt- Prozess in seine ggt-Prozess-Liste ein.
      NewConfig = keystore(ggts, Config, {ggts, GGTs ++ [Clientname]}),
      initial(NewConfig);
    step ->
      % Empfängt der Koordinator den Befehl “step” vom Benutzer,
      % so beendet er den “initial”-Zustand in dem er die ggT-Prozesse per Zufall in einem Ring anordnet.
      {_, GGTs} = keyfind(ggts, Config),
      {_, Cl} = keyfind(clients, Config),
      Llen = listLength(GGTs),
      util:logging(Logfile, "Anmeldefrist fuer ggT-Prozesse abgelaufen. Vemisst werden aktuell " ++ util:to_String(Cl -Llen) ++ " ggT-Prozesse\n"),
      %Die Liste der registrierten ggt-Prozesse mischen
      MixedGGTs = util:shuffle(GGTs),
      ringbildung(MixedGGTs, Config, []),
      util:logging(Logfile, "Ring wird/wurde erstellt, Koordinator geht in den Zustand 'Bereit fuer Berechnung'.\n"),
      %Daraufhin ist er im Zustand “bereit”.
      bereit(keystore(ggts, Config, {ggts, MixedGGTs}));
    %Der Koordinator kann durch den manuellen Befehl “kill” terminiert werden
    kill ->
	  {_, GGTs} = keyfind(ggts, Config),
	  {_, Koordinatorname} = keyfind(koordinatorname, Config),
	  {_, NameService} = keyfind(nameservice, Config),
      %Hier wechselt er dann in den Zustand “beenden”, wo er zuerst alle ggt-Prozesse beendet und dann sich selbst.
      beenden(GGTs, Config),
      util:logging(Logfile, "Alle GGTs wurden beendet und nun beendet sich auch der Koordinator. Tschüss.\n"),
      unregister(Koordinatorname),
      NameService ! {self(),{unbind, Koordinatorname}},
      %Beendet sich selbst
      erlang:exit(self(),normal);
    Any ->
      util:logging(Logfile, "Unerwartete Nachricht erhalten.\n" ++ util:to_String(Any) ++ " \n"),
      initial(Config)
  end.

%bereit
bereit(Config) ->
  {_, Logfile} = keyfind(logfile, Config),
  {_, GGTs} = keyfind(ggts, Config),
  {_, Korrigieren} = keyfind(korrigieren, Config),
  {_, NameService} = keyfind(nameservice, Config),
  {_, Koordinatorname} = keyfind(koordinatorname, Config),
  {_, MinMi} = keyfind(minmi, Config),
  receive
    reset ->
      %Beendet alle ggt-Prozesse (kill), leert die ggt-Prozessliste und setzt den Zustand auf initial.
      beenden(GGTs, Config),
      util:logging(Logfile, "Alle GGTs beendet und zurück auf initial-State\n"),
      initial(keystore(ggts, Config, {ggts, []}));
    prompt ->
      prompt_ggts(GGTs, Config),
      bereit(Config);
    %Empfängt die neue Mi des ggt und einen Timestamp.
    {briefmi,{Clientname,CMi,CZeit}} ->
      %Loggt dies.
      util:logging(Logfile, atom_to_list(Clientname) ++ " meldet neues Mi " ++ integer_to_list(CMi) ++ " um " ++ util:to_String(CZeit) ++ " (" ++ vsutil:now2string(erlang:timestamp()) ++ ")\n"),
      if
        %Falls diese Mi die bisher kleinste empfangene Mi ist, wird dies dementsprechend gespeichert.
        CMi < MinMi -> NewConfig = keystore(mi, Config, {mi, CMi}), bereit(NewConfig)
      end,
      bereit(Config);
    %Empfängt vom ggt das Ergebnis der beendeten Berechnung, sowie einen Timestamp.
    {From, briefterm, {Clientname,CMi,CZeit}} ->
      if
        %Dann überprüft der Koordinator ob die empfangene Mi größer als die gespeicherte kleinste Mi ist.
        CMi > MinMi ->
          if
            %Falls dies so ist und ‘korrigiere” 1 ist, teilt er dem ggt diese kleinste Zahl per “sendy” mit.
            Korrigieren == 1 ->
              From ! {sendy, MinMi},
              bereit(Config);
            true ->
              %Falls dies so ist und “korrigiere” 0 ist, schreibt er dies nur als Fehlermeldung in die Log-Datei.
              util:logging(Logfile, atom_to_list(Clientname) ++ " liefert fehlerhaftes Mi: " ++ integer_to_list(CMi) ++ " um " ++ util:to_String(CZeit) ++ " es ist bereits ein kleineres Mi bekannt: " ++ integer_to_list(MinMi) ++ "\n")
          end;
      	true ->
          %Ansonsten wird diese Mi als kleinste empfangene Mi gespeichert.
          NewConfig = keystore(mi, Config, {mi, CMi}), bereit(NewConfig)
      end;
    nudge ->
      nudge(GGTs, Config),
      bereit(Config);
    %Wechselt den Wert von “korrigiere” entweder von 0 auf 1 oder von 1 auf 0.
    toggle ->
      % Dies wird durch die Formel (“korrigiere” + 1) mod 2 getan
      bereit(keystore(korrigieren, Config, {korrigieren, ((Korrigieren+1) rem 2)}));
    {calc, WggT} ->
      util:logging(Logfile, "Beginne eine neue ggT-Berechnung mit Ziel " ++ util:to_String(WggT) ++ ".\n"),
      %Empfängt der Koordinator den Befehl “calc”, so informiert er die ggt-Prozesse über deren Mi-Wert
      %und startet bei 20% der Prozesse die Berechnung.
      Mis = vsutil:bestimme_mis(WggT, (length(GGTs))),
      calc(GGTs,Mis,Config),
      util:logging(Logfile, "Allen ggT-Prozessen ein initiales Mi gesendet.\n"),
      NoClients = max(2,trunc(length(GGTs)/5)),
      %Wählt 20% der ggt-Prozesse aus der ggt-Liste aus
      TwntyPrc = nthtail((length(GGTs)-NoClients), util:shuffle(GGTs)),
      Ys = vsutil:bestimme_mis(WggT, (length(TwntyPrc))),
      twentyPerc(TwntyPrc, Ys, Config),
      util:logging(Logfile, "Allen ausgewaehlten ggT-Prozessen ein y gesendet.\n"),
      bereit(Config);
    %Der Koordinator kann durch den manuellen Befehl “kill” terminiert werden
    kill ->
      %Hier wechselt er dann in den Zustand “beenden”, wo er zuerst alle ggt-Prozesse beendet und dann sich selbst.
      beenden(GGTs, Config),
      util:logging(Logfile, "Alle GGTs wurden beendet und nun beendet sich auch der Koordinator. Tschüss.\n"),
      NameService ! {self(),{unbind, Koordinatorname}},
      unregister(Koordinatorname),
      %Beendet sich selbst
      erlang:exit(self(),normal);
    Any ->
      util:logging(Logfile, "Unerwartete Nachricht erhalten.\n" ++ util:to_String(Any) ++ " \n"),
      initial(Config)
  end.

ringbildung([GGT | Tail], Config, GGTf) ->
  {_, Logfile} = keyfind(logfile, Config),
  {_, NameService} = keyfind(nameservice, Config),
  NameService ! {self(),{lookup, GGT}},
  receive
    not_found ->
      util:logging(Logfile, "GGT ist nicht auf dem Namensservice vorhanden\n");
    {pin, {Name,Node}} ->
      util:logging(Logfile, "ggT-Prozess " ++ util:to_String(GGT) ++ " (" ++ util:to_String(Name) ++ ") auf " ++ util:to_String(Node) ++ " gebunden.\n"),
      {Name,Node} ! {setneighbors, last(Tail), nth(1, Tail)},
      util:logging(Logfile, "ggT-Prozess " ++ util:to_String(Name) ++ " (" ++ util:to_String(Node) ++ ") ueber linken (" ++ atom_to_list(last(Tail)) ++ ") und rechten (" ++ atom_to_list(nth(1, Tail)) ++ ") Nachbarn informiert.\n")
      %Immer den ersten und letzten ggt_prozess durch setneighbors als Nachbarn setzen,
  end,
  GGTflength = listLength(GGTf),
  Taillength = listLength(Tail),
  if
    %Den ersten GGT der Liste ans Ende der Liste packen, bis dies mit allen einmal gemacht wurde.
    %Dies wird festgestellt, da alle GGTs die schon behandelt wurden in eine zweite Liste kommen, und wenn die gleichlang ist wie GGTs, wurden alle behandelt.
    GGTflength /= Taillength ->
    	ringbildung(Tail ++ [GGT], Config, GGTf ++ [GGT]);
    true ->
      util:logging(Logfile, "Alle ggT-Prozesse gebunden.\n"),
      util:logging(Logfile, "Alle ggT-Prozesse ueber Nachbarn informiert.\n")
  end.

prompt_ggts([], _) -> ok;
%Iteriert durch die ggt-Liste und erfragt bei jedem ggt seine aktuelle Mi (per “tellmi”) und loggt diese.
prompt_ggts([ GGT | Tail ], Config) ->
  {_, Logfile} = keyfind(logfile, Config),
  util:logging(Logfile, "prompt\n"),
  {_, NameService} = keyfind(nameservice, Config),
  NameService ! {self(),{lookup, GGT}},
  receive
    not_found -> util:logging(Logfile, "GGT ist nicht auf derm Namensservice vorhanden\n");
    {pin, GGTNameNode} ->
      GGTNameNode ! {self(), tellmi},
      receive
        {mi, Mi} ->
          util:logging(Logfile, atom_to_list(GGT) ++ " hat aktuellen Mi: " ++ integer_to_list(Mi) ++ "\n"),
          prompt_ggts(Tail, Config)
      end
  end.

%beenden
beenden([], Config) ->
  {_, Logfile} = keyfind(logfile, Config),
  util:logging(Logfile, "Allen ggT-Prozessen ein 'kill' gesendet.");
beenden([ GGT | Tail ], Config) ->
  {_, Logfile} = keyfind(logfile, Config),
  {_, NameService} = keyfind(nameservice, Config),
  NameService ! {self(),{lookup, GGT}},
  receive
    not_found ->
      util:logging(Logfile, "GGT ist nicht auf derm Namensservice vorhanden\n");
    {pin, GGTNameNode} ->
      GGTNameNode ! kill,
      beenden(Tail, Config)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

keyfind(_,[]) ->
  false;
keyfind(Key, Tuplelist) ->
  [Head|Rest] = Tuplelist,
  {K,_} = Head,
  if K == Key -> Head;
    true -> keyfind(Key,Rest)
  end.

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

listLength([]) ->
  0;
listLength(L) ->
  listLength(L,0).

listLength([],A) ->
  A;
listLength([_|T],A) ->
  listLength(T,A+1).

last([]) ->
  [];
last([H]) ->
  H;
last([_|T]) ->
  last(T).

nth(1,[H]) ->
  H;
nth(1,[H|_]) ->
  H;
nth(N,[_|T]) ->
  nth(N-1, T).

nthtail(0, L) ->
  L;
nthtail(1,[_]) ->
  [];
nthtail(1,[_|T]) ->
  T;
nthtail(N,[_|T]) ->
  nthtail(N-1, T).

nudge([],_) ->
  ok;
nudge([H],Config) ->
  {_, NameService} = keyfind(nameservice, Config),
  {_, Logfile} = keyfind(logfile, Config),
  %Iteriert durch die ggt-Liste und erfragt bei jedem ggt seinen aktuelle Lebenszustand (per “pingGGT”) und loggt diesen.
  NameService ! {self(),{lookup, H}},
  receive
    not_found ->
      util:logging(Logfile, "GGT ist nicht auf dem Namensservice vorhanden\n");
  %Sendet diesen ggt-Prozessen eine Zahl
    {pin, {_,GGTNode}} ->
      GGTNode ! {self(), pingGGT},
      receive
        {pongGGT,GGTName} ->
          util:logging(Logfile, "Lebenszustand von " ++ util:to_String(GGTName) ++ " ist lebendig.\n")
      after
        500 ->
          util:logging(Logfile, "Lebenszustand von " ++ atom_to_list(H) ++ " ist nicht auffindbar\n")
      end
  end;
nudge([H|T],Config) ->
  {_, NameService} = keyfind(nameservice, Config),
  {_, Logfile} = keyfind(logfile, Config),
  %Iteriert durch die ggt-Liste und erfragt bei jedem ggt seinen aktuelle Lebenszustand (per “pingGGT”) und loggt diesen.
  NameService ! {self(),{lookup, H}},
  receive
    not_found ->
      util:logging(Logfile, "GGT ist nicht auf dem Namensservice vorhanden\n");
    %Sendet diesen ggt-Prozessen eine Zahl
    {pin, {_,GGTNode}} ->
      GGTNode ! {self(), pingGGT},
      receive
        {pongGGT,GGTName} ->
          util:logging(Logfile, "Lebenszustand von " ++ util:to_String(GGTName) ++ " ist lebendig.\n")
      after
        500 ->
          util:logging(Logfile, "Lebenszustand von " ++ atom_to_list(H) ++ " ist nicht auffindbar\n")
      end,
      nudge(T,Config)
    end.


calc([H],Mis,Config) ->
  {_, NameService} = keyfind(nameservice, Config),
  {_, Logfile} = keyfind(logfile, Config),
  CMi = nth(index_of(H, [H]),Mis),
  NameService ! {self(),{lookup, H}},
  receive
    not_found ->
      util:logging(Logfile, "GGT ist nicht auf dem Namensservice vorhanden\n");
  %Sendet diesen ggt-Prozessen eine Zahl
    {pin, {Name,Node}} ->
      {Name,Node} ! {setpm, CMi},
      util:logging(Logfile, "ggT-Prozess " ++ util:to_String(Name) ++ " (" ++ util:to_String(Node) ++ ") initiales Mi " ++ util:to_String(CMi) ++ " gesendet.\n")
  end;
calc([H|T],[HMi|TMi],Config) ->
  {_, NameService} = keyfind(nameservice, Config),
  {_, Logfile} = keyfind(logfile, Config),
    CMi = nth(index_of(H, [H|T]),[HMi|TMi]),
    NameService ! {self(),{lookup, H}},
    receive
      not_found ->
        util:logging(Logfile, "GGT ist nicht auf dem Namensservice vorhanden\n");
    %Sendet diesen ggt-Prozessen eine Zahl
      {pin, {Name,Node}} ->
        {Name,Node} ! {setpm, CMi},
        util:logging(Logfile, "ggT-Prozess " ++ util:to_String(Name) ++ " (" ++ util:to_String(Node) ++ ") initiales Mi " ++ util:to_String(CMi) ++ " gesendet.\n"),
        calc(T,TMi,Config)
    end.


twentyPerc([H],Ys,Config) ->
  {_, NameService} = keyfind(nameservice, Config),
  {_, Logfile} = keyfind(logfile, Config),
  NameService ! {self(),{lookup, H}},
  Y = nth(index_of(H, [H]),Ys),
  receive
    not_found ->
      util:logging(Logfile, "GGT ist nicht auf dem Namensservice vorhanden\n");
  %und startet mit sendy rekursiv die Berechnung.
    {pin, {Name,Node}} ->
      {Name,Node} ! {sendy, Y},
      util:logging(Logfile, "ggT-Prozess " ++ util:to_String(Name) ++ " (" ++ util:to_String(Node) ++ ") startendes y " ++ util:to_String(Y) ++ " gesendet.\n")
  end;
twentyPerc([H|T],[HYs|TYs],Config) ->
  {_, NameService} = keyfind(nameservice, Config),
  {_, Logfile} = keyfind(logfile, Config),
    NameService ! {self(),{lookup, H}},
    Y = nth(index_of(H, [H|T]),[HYs|TYs]),
    receive
      not_found ->
        util:logging(Logfile, "GGT ist nicht auf dem Namensservice vorhanden\n");
    %und startet mit sendy rekursiv die Berechnung.
      {pin, {Name,Node}} ->
        {Name,Node} ! {sendy, Y},
        util:logging(Logfile, "ggT-Prozess " ++ util:to_String(Name) ++ " (" ++ util:to_String(Node) ++ ") startendes y " ++ util:to_String(Y) ++ " gesendet.\n"),
        twentyPerc(T,TYs,Config)
    end.

msg(Msg) ->
  {ok, File} = file:consult("koordinator.cfg"),
  {ok, NameServiceNode} = vsutil:get_config_value(nameservicenode, File),
  {ok, Koordinatorname} = vsutil:get_config_value(koordinatorname, File),
  pong = net_adm:ping(NameServiceNode),
  NameService = global:whereis_name(nameservice),
  NameService ! {self(),{lookup, Koordinatorname}},
  receive
    not_found ->
      ok;
    {pin, {Name, Node}} ->
      {Name, Node} ! Msg
  end.