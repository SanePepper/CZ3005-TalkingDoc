%The command to start the session.
entry() :- reset(), moodAssessment(), painAssessment(), matchPainLevel(), 
	mood(M), pain(A,B), 
	format("So you feel ~w and have ~w pain which has a pain level of ~w.", [M,A,B]), 
	nl(), diagnose().

/* 
	There is a total of 6 diagnosable diseases.
	Predicate name:  Name of the disease
	First argument:  Serious level of the disease. Affect the gesture of doctor
	Second argument: Standard pain level of the disease
	Third argument:  List of symptoms of the disease
*/
coronavirus(1,0,[body_ache,breathlessness,cough,diarrhea,headache,high_temperature]).
food_poison(0,3,[body_ache,dehydration,diarrhea,nausea,rapid_heart_beat]).
injury(1,4,[bleeding,body_ache,bruises,dizziness,headache,nausea]).
fever(0,1,[body_ache,high_temperature,shivering,sweating,rapid_heart_beat]).
cold(-1,0,[cough,dehydration,headache,high_temperature,sneeze,sweating,runny_nose]).
low_blood_sugar(-1,0,[dehydration,headache,nausea,shivering,rapid_heart_beat]).

%Counters used in the program
%Percepted serious level of the illness. Affect gesture of the doctor
seriousLevel(0). 	
%Likelihood of different illnesses. Differentiated bt first two character
cv_count(0). %Coronavirus
fp_count(0). %Food Poisoning
ij_count(0). %Injury
fv_count(0). %Fever
cd_count(0). %Cold
lb_count(0). %Low Blood Sugar

%Rules to update counters
adjSerious(N) :- seriousLevel(X), Y is X+N, retract(seriousLevel(X)), assert(seriousLevel(Y)),!.	
cv_adj(N) :- cv_count(X), Y is X+N, retract(cv_count(X)), assert(cv_count(Y)),!.
fp_adj(N) :- fp_count(X), Y is X+N, retract(fp_count(X)), assert(fp_count(Y)),!.
ij_adj(N) :- ij_count(X), Y is X+N, retract(ij_count(X)), assert(ij_count(Y)),!.
fv_adj(N) :- fv_count(X), Y is X+N, retract(fv_count(X)), assert(fv_count(Y)),!.
cd_adj(N) :- cd_count(X), Y is X+N, retract(cd_count(X)), assert(cd_count(Y)),!.
lb_adj(N) :- lb_count(X), Y is X+N, retract(lb_count(X)), assert(lb_count(Y)),!.

% Remove all records of the counters and insert new record with their default value
reset() :- retractall(seriousLevel(_)), assert(seriousLevel(0)), 
	retractall(symptom(_,_)), assert(symptom(nothing, n)), 
	retractall(cv_count(_)), assert(cv_count(0)), retractall(fp_count(_)), assert(fp_count(0)),	
	retractall(ij_count(_)), assert(ij_count(0)), retractall(fv_count(_)), assert(fv_count(0)), 
	retractall(cd_count(_)), assert(cd_count(0)), retractall(lb_count(_)), assert(lb_count(0)).

/* 
	The mood_library contains a list of all possible mood of the patient
	For each element in the list, first argument describes the mood 
		and second argument is the influence on the doctor's gesture
*/
mood_library([[peaceful|-2], [calm|-1], [content|-1], [angry|3], [weepy|1], 
	[stressed|1], [irritated|2], [cranky|2]]).

/* 	Mood Assessment 
	A random sequence of question will be generated	*/
moodAssessment() :- retractall(mood(_)), % Have to clear the memory before it starts
	mood_library(Lib), random_permutation(Lib, MoodSeq), askMood(MoodSeq).

/* 	Given a list of mood, ask about the first element and remove from the list 
	M: Description of the mood
	X: Effect on the serious level of the mood
	T: Remaining options	*/
askMood(L) :- expressGesture(), L = [[M|X]|T], 
	format("Do you feel ~w ? y/n/q: ", [M]), read(Ans),
	(Ans==q -> abort; Ans==y -> assert(mood(M)), adjSerious(X); % If answers yes, update knowledge base
		(T=[] -> print("You are not sure about your mood. Let me ask again."), nl(), moodAssessment(); 
		%If answers no and no more choices, restart the assesment
			askMood(T))). % If answers no, move on to the next choice
/* 
	The pain_library contains a list of all possible pain levels
	For each element in the list, first argument describes the pain level 
		and second argument is the respective pain level in numerical scale
*/
pain_library([[worst_possible|5], [unbearable|5], [severe|4], [intense|4], 
	[lot_of|3], [moderate|2], [discomforting|2], [manageable|1], [mild|1]]). 

/* 	Pain Assessment 
	First ask whether the patient have any pain	*/
painAssessment() :- expressGesture(), retractall(pain(_,_)), % Clear previous memory about pain
	print("Do you feel any pain? y/n/q:"), read(Ans), (Ans==q -> abort; 
	Ans==y -> print("From mild pain to worst possible pain, please tell me your pain level."), nl(), 
		pain_library(P), askPain(P); 		% Answer yes, start to ask for the pain level
	assert(pain(no,0)), adjSerious(-3)). 	% Answer no, patient do not have pain

/* 	Given a list of pain levels, perform binary search */
askPain(L) :- getMid(L,[M|X]), % Select the middle choice from the list
	format("Do you have ~w pain? y/n/q: ", [M]), read(Ans), (Ans==q -> abort; 
		Ans==y -> assert(pain(M,X)), adjSerious(X); 	% Answer yes, record the pain level
		length(L,Len), Len>1 -> comparePain(L,[M|X]); 	% Answer no, ask for comparision
			reassess()).		% Answer no and no more options, re-assess the pain level

comparePain(L,[M|X]) :- format("Is it more intense than ~w pain? y/n/q: ", [M]), read(Ans),
	(Ans==q -> abort; 
		Ans==y -> getFront(L,[M|X],N), askPain(N); 	% N: Options with more intense pain
		getBehind(L,[M|X],N), askPain(N)).			% N: Options with less intense pain

reassess() :- print("You are not sure about your pain level. Let me ask again."), nl(), 
	pain_library(P), askPain(P).

/* 	Obtain the middle element from the list 	
	Recursion: Remove one element at the front and back each time
	[_|L1] contains the list without the last element
	L1 contains the list without the first and last element
	Perform recursion to trim the list until it has 1 or 2 elements	
*/
getMid([L],L). 		% Only one element in the list
getMid([L,_|[]],L). % Only two elements in the list
getMid(L,M) :- removeLast(L,[_|L1]), getMid(L1,M), !.

/* 	Remove the last element from the list	
	Recursion: If there are more than one element in the list,
	Copy the first element to the front of the buffer.
	When the function returns, the previous element would be insert 
	before the current element to preserve the order of the list
*/	
removeLast([_],[]). % Only one the last element remains, remove and return empty list
removeLast([A|B],[A|C]) :- removeLast(B,C).

/* 	Obtain the list of elements before element M.
	Append the first element to the buffer when
	the first element does not match with M.
	Return the buffer list N.	*/
getFront([H|T],H,[]) :- !.
getFront([H|T],M,[H|N]) :- getFront(T,M,N), !.

/*	Obtain the list of elements after element M.
	Remove the first element of the list,
	until the first element matches with M.
	Return the remaining list N. 	*/
getBehind([H|T],H,T) :- !.
getBehind([_|T],M,N) :- getBehind(T,M,N), !.

% Compare the experienced pain level with the standard pain level of each disease.	
matchPainLevel() :- pain(_,P), check_cv(P),check_lb(P), 
	check_cd(P), check_fv(P), check_ij(P), check_fp(P), !.

/*	Increase the likelihood of disease diagnosis by 1
		if the pain level is similar to the standard
	P: Pain level of patient
	S: Standard pain level of the disease 	*/
check_cv(P) :- coronavirus(_,S,_), abs(P-S,X), X =< 1 -> cv_adj(1); true.
check_fp(P) :- food_poison(_,S,_), abs(P-S,X), X =< 1 -> fp_adj(1); true.
check_ij(P) :- injury(_,S,_), abs(P-S,X), X =< 1 -> ij_adj(1); true.
check_fv(P) :- fever(_,S,_), abs(P-S,X), X =< 1 -> fv_adj(1); true.
check_cd(P) :- cold(_,S,_), abs(P-S,X), X =< 1 -> cd_adj(1); true.
check_lb(P) :- low_blood_sugar(_,S,_), abs(P-S,X), X =< 1 -> lb_adj(1); true.

/*	List of gestures performed by doctor at different attitudes.
	Normal gesture by default. 
	Calming gesture if doctor gets serious.
	Polite gesture if doctor relaxes (less serious). 	*/
polite_gesture([look_concerned, mellow_voice, light_touch, faint_smile, smile]).
normal_gesture([broad_smile, joke, beaming_voice, writes_on_paper, adjust_mask]).
calming_gesture([greet, look_composed, look_attentive, pat_on_shoulder, deep_breath]).

/* 	Doctor get serious if serious level > 2.
	Becomes relaxed if serious level < -2. 
	A random gesture that fits his attitude is performed.	*/
expressGesture() :- seriousLevel(X), (X < -2 -> polite_gesture(L); 
	(X > 2 -> calming_gesture(L); normal_gesture(L))), 
	random_member(E, L), format("(~w) ",[E]).

possibleSymptoms(16). % Total number of possible symptoms
symptom(nothing, n).  % No known symptom initially

% Obtain values of counter variable
status([A,B,C,D,E,F],S) :- cv_count(A), fp_count(B), ij_count(C), 
	fv_count(D), cd_count(E), lb_count(F), seriousLevel(S), !.

% Perform actions if sufficient information is obtained
% Otherwise, would ask for symptoms to gather more information
diagnose() :- expressGesture(), sufficientInfo(), !. 
diagnose() :- askSymptom().

% Arrive to a decision if the likelihood level reach the threshold,
% 	when all diseases have very low likelihood,
%	or when all symptoms are asked and no decision is made
sufficientInfo() :- status(X,Y), max_list(X,Max), %print(X), print(Y), 
	not(between(-1, 4, Max)) -> giveResult(Max); % Likelihood level is too high or too low
	queryHistory(H), length(H, Hs), Hs = 17 -> giveResult(0). % Exhausted all symptoms

% Ask if the patient has a specific symptom S
% Record his answer and adjust the assessment base on the answer
askSymptom() :- mostProbableSymptom(S), 
	format("Do you have ~w? y/n/q: ",[S]), read(Ans),
	(Ans==q -> abort; Ans==y -> assert(symptom(S,y)), matchDisease(S,1); 
		assert(symptom(S,n)), matchDisease(S,-1)), 
	diagnose().	% Continue the diagnosis after asking a question

% Diseases with symptom S would adjust its likelihood level by N.
%	L_: List of symptom for respective diseases, used for finding matches with S
%	X_: The serious level of each disease, 
% 		doctor would changes his response as the likelihood for certain disease changes
matchDisease(S,N) :- coronavirus(X1,_,L1), (member(S,L1) -> adjSerious(X1*N), cv_adj(N); true),
	food_poison(X2,_,L2), (member(S,L2) -> adjSerious(X2*N), fp_adj(N); true),
	injury(X3,_,L3), (member(S,L3) -> adjSerious(X3*N), ij_adj(N); true),
	fever(X4,_,L4), (member(S,L4) -> adjSerious(X4*N), fv_adj(N); true),
	cold(X5,_,L5), (member(S,L5) -> adjSerious(X5*N), cd_adj(N); true),
	low_blood_sugar(X6,_,L6), (member(S,L6) -> adjSerious(X6*N), lb_adj(N); true).

/* 	Randomly pick one symptom from the symptom list of most likely diagnosis.	*/
mostProbableSymptom(S) :- status(X,_), max_list(X,Max), %Max: The highest likelihood level
	% L_ would store the symptom list of each disease if they have the highest likelihood
	% Otherwise, L_ would be empty 
	cv_count(C1), (C1 = Max -> coronavirus(_,_,L1); L1=[]),
	fp_count(C2), (C2 = Max -> food_poison(_,_,L2); L2=[]),
	ij_count(C3), (C3 = Max -> injury(_,_,L3); L3=[]),
	fv_count(C4), (C4 = Max -> fever(_,_,L4); L4=[]),
	cd_count(C5), (C5 = Max -> cold(_,_,L5); L5=[]),
	lb_count(C6), (C6 = Max -> low_blood_sugar(_,_,L6); L6=[]),
	% L123456 is the concatanation of L1-L6.
	% L is the set which eliminates duplicate entries in L123456
	append(L1,L2,L12), append(L12,L3,L123), append(L123,L4,L1234), 
	append(L1234,L5,L12345), append(L12345,L6,L123456), list_to_set(L123456,L), 
	queryHistory(H), 	% H is a list of symptom that has been asked
	subtract(L,H,P), 	% P is the remaining possible symptoms
	% If P is empty, all symptoms of the highly likely diseases were asked
	% Should eliminate these options
	(P == [] -> removeChoices(Max), mostProbableSymptom(S); 
	% If P is not empty, pick a random item from P
	random_member(S,P)).

% Generate a list of symptoms based on the query history
queryHistory(H) :- findall(X,symptom(X,_),H).

% Significantly reduce the likelihood for diseases with specific likelihood level.
removeChoices(N) :- (cv_count(N) -> cv_adj(-20); true),
	(fp_count(N) -> fp_adj(-20); true),
	(ij_count(N) -> ij_adj(-20); true),
	(fv_count(N) -> fv_adj(-20); true),
	(cd_count(N) -> cd_adj(-20); true),
	(lb_count(N) -> lb_adj(-20); true).

% Announce the result. 
% N is the highest likelihood level among the diseases
% N>=5 represents a disease is diagnosed, patient can be diagnosed with multiple diseases
% Any smaller values represents that they do not have the disease
giveResult(N) :- N >= 5 -> 
   	(cv_count(N) -> print("You have COVID-19. Please go to the hospital for quarantine."), nl(); true),
	(fp_count(N) -> print("You have food poisioning. Please have some rest."), nl(); true),
	(ij_count(N) -> print("You are injuried. The nurse would treat your wound."), nl(); true),
	(fv_count(N) -> print("You have fever. Take these pills."), nl(); true),
	(cd_count(N) -> print("You have a cold. Take these pills."), nl(); true),
	(lb_count(N) -> print("You have low blood sugar level. Have some food and have a rest."), nl(); true);
	print("You do not have any illness. You can go home now."). % Can go home if they do not have any disease