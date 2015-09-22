%PAKCS1.11 swi6 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').


:-curryModule('Program1').

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('Program1.append1',append1,2,'Program1.append1',nofix,'FuncType'('TCons'([],[_G99042]),'FuncType'('TCons'([],[_G99042]),'TCons'([],[_G99042])))).
functiontype('Program1.top',top,1,'Program1.top',nofix,'FuncType'('TCons'('Program1.Stack',['TCons'('Program1.Stack',[_G100343])]),'TCons'('Program1.Stack',[_G100343]))).
functiontype('Program1.pop',pop,1,'Program1.pop',nofix,'FuncType'('TCons'('Program1.Stack',[_G101629]),'TCons'('Program1.Stack',[_G101629]))).
functiontype('Program1.member',member,2,'Program1.member',nofix,'FuncType'(_G102852,'FuncType'('TCons'([],[_G102852]),'TCons'('Prelude.Bool',[])))).
functiontype('Program1.insert',insert,2,'Program1.insert',nofix,'FuncType'(_G104072,'FuncType'('TCons'([],[_G104072]),'TCons'([],[_G104072])))).
functiontype('Program1.main',main,0,'Program1.main',nofix,'TCons'('Prelude.IO',['TCons'('Prelude.()',[])])).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.
constructortype('Program1.Empty','Empty',0,'Empty',0,'TCons'('Program1.Stack',[_G106580])).
constructortype('Program1.Push','Push',2,'Push',1,'FuncType'(_G106728,'FuncType'('TCons'('Program1.Stack',[_G106728]),'TCons'('Program1.Stack',[_G106728])))).

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'Program1.append1'(_G109130,_G109131,_G109132,_G109133,_G109134):-freeze(_G109133,'blocked_Program1.append1'(_G109130,_G109131,_G109132,_G109133,_G109134)).
'blocked_Program1.append1'(_G109173,_G109182,_G109606,_G109609,_G109612):-hnf(_G109173,_G110092,_G109609,_G110080),'blocked_Program1.append1_1'(_G110092,_G109182,_G109606,_G110080,_G109612).

'blocked_Program1.append1_1'(_G110241,_G110242,_G110243,_G110244,_G110245):-freeze(_G110244,'blocked_blocked_Program1.append1_1'(_G110241,_G110242,_G110243,_G110244,_G110245)).
'blocked_blocked_Program1.append1_1'([],_G109182,_G110343,_G110346,_G110349):-hnf(_G109182,_G110343,_G110346,_G110349).
'blocked_blocked_Program1.append1_1'([_G109275|_G109284],_G109182,_G110595,_G110598,_G110601):-!,hnf('Prelude.++'([_G109275],'Program1.append1'(_G109284,_G109182)),_G110595,_G110598,_G110601).
'blocked_blocked_Program1.append1_1'('FAIL'(_G111301),_G109182,'FAIL'(_G111301),_G111308,_G111308):-nonvar(_G111301).

'Program1.top'(_G111631,_G111632,_G111633,_G111634):-freeze(_G111633,'blocked_Program1.top'(_G111631,_G111632,_G111633,_G111634)).
'blocked_Program1.top'(_G111669,_G111862,_G111865,_G111868):-hnf(_G111669,_G112266,_G111865,_G112257),'blocked_Program1.top_1'(_G112266,_G111862,_G112257,_G111868).

'blocked_Program1.top_1'(_G112402,_G112403,_G112404,_G112405):-freeze(_G112404,'blocked_blocked_Program1.top_1'(_G112402,_G112403,_G112404,_G112405)).
'blocked_blocked_Program1.top_1'('Program1.Empty','Program1.Empty',_G112604,_G112604).
'blocked_blocked_Program1.top_1'('Program1.Push'(_G111769,_G111778),_G112967,_G112970,_G112973):-!,hnf(_G111769,_G112967,_G112970,_G112973).
'blocked_blocked_Program1.top_1'('FAIL'(_G113177),'FAIL'(_G113177),_G113184,_G113184):-nonvar(_G113177).

'Program1.pop'(_G113503,_G113504,_G113505,_G113506):-freeze(_G113505,'blocked_Program1.pop'(_G113503,_G113504,_G113505,_G113506)).
'blocked_Program1.pop'(_G113541,_G113734,_G113737,_G113740):-hnf(_G113541,_G114138,_G113737,_G114129),'blocked_Program1.pop_1'(_G114138,_G113734,_G114129,_G113740).

'blocked_Program1.pop_1'(_G114274,_G114275,_G114276,_G114277):-freeze(_G114276,'blocked_blocked_Program1.pop_1'(_G114274,_G114275,_G114276,_G114277)).
'blocked_blocked_Program1.pop_1'('Program1.Empty','Program1.Empty',_G114476,_G114476).
'blocked_blocked_Program1.pop_1'('Program1.Push'(_G113641,_G113650),_G114839,_G114842,_G114845):-!,hnf(_G113650,_G114839,_G114842,_G114845).
'blocked_blocked_Program1.pop_1'('FAIL'(_G115049),'FAIL'(_G115049),_G115056,_G115056):-nonvar(_G115049).

'Program1.member'(_G115429,_G115430,_G115431,_G115432,_G115433):-freeze(_G115432,'blocked_Program1.member'(_G115429,_G115430,_G115431,_G115432,_G115433)).
'blocked_Program1.member'(_G115472,_G115481,_G115908,_G115911,_G115914):-hnf(_G115481,_G116376,_G115911,_G116364),'blocked_Program1.member_2'(_G116376,_G115472,_G115908,_G116364,_G115914).

'blocked_Program1.member_2'(_G116522,_G116523,_G116524,_G116525,_G116526):-freeze(_G116525,'blocked_blocked_Program1.member_2'(_G116522,_G116523,_G116524,_G116525,_G116526)).
'blocked_blocked_Program1.member_2'([],_G115472,'Prelude.False',_G116627,_G116627).
'blocked_blocked_Program1.member_2'([_G115581|_G115590],_G115472,_G116929,_G116932,_G116935):-!,makeShare(_G115472,_G117002),hnf('Prelude.||'('Prelude.=='(_G117002,_G115581),'Program1.member'(_G117002,_G115590)),_G116929,_G116932,_G116935).
'blocked_blocked_Program1.member_2'('FAIL'(_G117823),_G115472,'FAIL'(_G117823),_G117830,_G117830):-nonvar(_G117823).

'Program1.insert'(_G118207,_G118208,_G118209,_G118210,_G118211):-freeze(_G118210,'blocked_Program1.insert'(_G118207,_G118208,_G118209,_G118210,_G118211)).
'blocked_Program1.insert'(_G118250,_G118259,_G118875,_G118878,_G118881):-hnf(_G118259,_G119403,_G118878,_G119391),'blocked_Program1.insert_or1_2'(_G119403,_G118250,_G118875,_G119391,_G118881).

'blocked_Program1.insert_or1_2'(_G119561,_G119562,_G119563,_G119564,_G119565):-freeze(_G119564,'blocked_blocked_Program1.insert_or1_2'(_G119561,_G119562,_G119563,_G119564,_G119565)).
'blocked_blocked_Program1.insert_or1_2'([_G118316|_G118325],_G118250,[_G118316|'Program1.insert'(_G118250,_G118325)],_G119717,_G119717):-!.
'blocked_blocked_Program1.insert_or1_2'([],_G118250,_G120298,_G120301,_G120304):-!,hnf('Prelude.failure'('Program1.insert',[[]]),_G120298,_G120301,_G120304).
'blocked_blocked_Program1.insert_or1_2'('FAIL'(_G120831),_G118250,'FAIL'(_G120831),_G120838,_G120838).
'blocked_Program1.insert'(_G118250,_G118259,[_G118250|_G118259],_G120865,_G120865).

'Program1.main'(_G121447,_G121448,_G121449):-freeze(_G121448,'blocked_Program1.main'(_G121447,_G121448,_G121449)).
'blocked_Program1.main'(_G121890,_G121893,_G121896):-hnf('Prelude.print'(['^H','^e','^l','^l','^o']),_G121890,_G121893,_G121896).

:-costCenters(['']).




%%%%% Number of shared variables: 1
