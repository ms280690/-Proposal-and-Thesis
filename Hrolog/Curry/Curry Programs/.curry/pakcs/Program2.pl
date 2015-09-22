%PAKCS1.11 swi6 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').


:-curryModule('Program2').

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('Program2.my_append',my_append,3,'Program2.my_append',nofix,'FuncType'('TCons'([],[_G45893]),'FuncType'('TCons'([],[_G45893]),'FuncType'('TCons'([],[_G45893]),'TCons'('Prelude.Success',[]))))).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'Program2.my_append'(_G48936,_G48937,_G48938,_G48939,_G48940,_G48941):-freeze(_G48940,'blocked_Program2.my_append'(_G48936,_G48937,_G48938,_G48939,_G48940,_G48941)).
'blocked_Program2.my_append'(_G48984,_G48993,_G49002,_G49809,_G49812,_G49815):-hnf(_G48984,_G50341,_G49812,_G50326),'blocked_Program2.my_append_1'(_G50341,_G48993,_G49002,_G49809,_G50326,_G49815).

'blocked_Program2.my_append_1'(_G50497,_G50498,_G50499,_G50500,_G50501,_G50502):-freeze(_G50501,'blocked_blocked_Program2.my_append_1'(_G50497,_G50498,_G50499,_G50500,_G50501,_G50502)).
'blocked_blocked_Program2.my_append_1'([],_G48993,_G49002,_G50604,_G50607,_G50610):-hnf('Prelude.=:='(_G48993,_G49002),_G50604,_G50607,_G50610).
'blocked_blocked_Program2.my_append_1'([_G49168|_G49177],_G48993,_G49002,_G51224,_G51227,_G51230):-!,hnf(_G49002,_G51972,_G51227,_G51954),'blocked_blocked_Program2.my_append_1_._4'(_G51972,_G49168,_G49177,_G48993,_G51224,_G51954,_G51230).

'blocked_blocked_Program2.my_append_1_._4'(_G52174,_G52175,_G52176,_G52177,_G52178,_G52179,_G52180):-freeze(_G52179,'blocked_blocked_blocked_Program2.my_append_1_._4'(_G52174,_G52175,_G52176,_G52177,_G52178,_G52179,_G52180)).
'blocked_blocked_blocked_Program2.my_append_1_._4'([_G49237|_G49246],_G49168,_G49177,_G48993,_G52337,_G52340,_G52343):-!,hnf('Prelude.cond'('Prelude.=:='(_G49168,_G49237),'Program2.my_append'(_G49177,_G48993,_G49246)),_G52337,_G52340,_G52343).
'blocked_blocked_blocked_Program2.my_append_1_._4'([],_G49168,_G49177,_G48993,_G53373,_G53376,_G53379):-!,hnf('Prelude.failure'('Program2.my_append',[[]]),_G53373,_G53376,_G53379).
'blocked_blocked_blocked_Program2.my_append_1_._4'('FAIL'(_G54018),_G49168,_G49177,_G48993,'FAIL'(_G54018),_G54025,_G54025).
'blocked_blocked_Program2.my_append_1'('FAIL'(_G54060),_G48993,_G49002,'FAIL'(_G54060),_G54067,_G54067):-nonvar(_G54060).

:-costCenters(['']).




%%%%% Number of shared variables: 0
