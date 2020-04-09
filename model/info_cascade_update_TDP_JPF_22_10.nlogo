;; 2-D representation of a belief propagation network written by Toby D. Pilditch and Jan-Philipp Fränken (2019)


;;##################################################
; EXTENSIONS
;;##################################################


extensions [
  stats
]


;;##################################################
; VARIABLES
;;##################################################


globals [
  linked-neighbours       ;; how many agents an agent is linked to
  number-agents           ;; total number of agents
  neutral-agents          ;; agents that have not committed to an opinion (keeping their prior value)
  opinion-A-agents        ;; agents holding opinion A
  opinion-B-agents        ;; agents holding opinion B
  match-counter           ;; checking whether number of  A agents = number of B agents and counts how many times there was no change (i.e. totals have been the same)
  tick-tot-odd            ;; counters used to check when no further learning takes place (i.e. if tick-tot-odd and tick-tot-even are equal)
  tick-tot-even
  peak-spread
  cl-prop-same            ;; mean of proportion of like-minded out of total neighbours
  cl-prop-diff            ;; mean of proportion of anti-minded out of total neighbours

]

turtles-own [
  p-H                     ;; prior belief of agent (i.e. starting point for learning)
  communication-direction ;; variable that stores the direction an agent has been 'pushed' to (i.e. what it was told by its neighbor before updating); it dictates the flipping in the BMSC function
  p-E-mean-red            ;; stores mean p-E of neighbors holding opinion 1
  p-E-mean-blue           ;; stores mean p-E of neighbors holding opinion 2
  p-T-mean-red            ;; stores mean p-T of neighbors holding opinion 1
  p-T-mean-blue           ;; stores mean p-T of neighbors holding opinion 2
  SCval1                  ;; count of neighbors holding same opinion as communicating agent or target agent dependent on the conditional used
  SCval2                  ;; count of neighbors holding different opinion as communicating agent or target agent dependent on the conditional used
  p-H-R                   ;; posterior belief of agent dictating posterior-opinion
  posterior-opinion       ;; posterior opinion after learning process
  prior-con               ;; confidence in prior-opinion
  p-E                     ;; p-Expertise of persuader / other agent that communicates opinion
  p-T                     ;; p-Trustworthiness of persuader / other agent that communicates opinion
  p-E-weighted            ;; conformity weighted expertise value used in the BSCM
  p-T-weighted            ;; conformity weighted trustworthiness value used in the BSCM
  posterior-con           ;; confidence in posterior-opinion
  learn-parameter         ;; learning parameter influencing speed of learning


  network-size            ;; size of each agent's network (i.e. number of links)
  opinion-threshold       ;; threshold that defines individual levels of scepticism
  recieve-belief          ;; checks whether the learner has received an opinion from the last tick
  belief-state            ;; whether the agent holds an opinion (0 = neutral agent, 1 = opinion-A-agent, 2 = opinion-B-agent)
  com-num                 ;; counts how often a learner has communicated

  clust-num-same          ;; value for number of "like-minded" linked neighbours
  clust-num-diff          ;; value for number of "anti-minded" linked neighbours
  clust-prop-same         ;; proportion of like-minded out of total neighbours
  clust-prop-diff         ;; proportion of anti-minded out of total neighbours
  nearest-learners

  SC-sensitivity-bel      ;; individual agent sensitivity to believe based on conformity motivations (THEORY VALIDATION)
  SC-sensitivity-com      ;; individual agent sensitivity to communicate based on conformity motivations

]

patches-own []

breed [learners learner]
breed [ex-learners ex-learner]



;;##################################################
;; SETUP
;;##################################################


to setup
  clear-all
  setup-patches          ;; function setting up patches
  setup-turtles          ;; function setting up turtles
  reset-ticks
end


to setup-patches
  ask patches [ set pcolor black ]
end


to setup-priors
;; setting up P(H), p(E), and P(T)
  ask turtles[
  set p-H random-normal prior-mean prior-sd
  set p-E random-normal prior-mean prior-sd
  set p-T random-normal prior-mean prior-sd
  while [p-H > 1 or p-H < 0][
    set p-H random-normal prior-mean prior-sd]
  while [p-E > 1 or p-E < 0][
    set p-E random-normal prior-mean prior-sd]
  while [p-T > 1 or p-T < 0][
    set p-T random-normal prior-mean prior-sd]]
end


to setup-links
 let link-counter 0
  ask turtles [
    ifelse prox-YN [
      set nearest-learners min-n-of network-size (other turtles) [ distance myself ]
    ][
      set nearest-learners n-of network-size (other turtles)]
     let near-learners turtle-set (nearest-learners)
    create-links-with near-learners
  ]
   ask links
  [set color grey - 1
   set thickness 0.1]
end


to setup-turtles
  ifelse scale-free = TRUE [                    ;; Scale free network setup make the initial network of two turtles and an edge
  make-node nobody                              ;; first node, unattached
  make-node turtle 0                            ;; second node, attached to first node
  let SF-create-count 2
  while [SF-create-count < n_agents]            ;;loop for generating scale-free network
  [ask links [ set color gray ]
  make-node find-partner
  layout
  set SF-create-count (SF-create-count + 1)
  ]
  ask turtles
     [set-characteristics
      ]
   ask links                                   ;; Severing "builder" links
   [die]
  ]
  [
  create-learners n_agents                     ;; random network
    [ setxy ((random (2 * max-pxcor)) + min-pxcor)
            ((random (2 * max-pycor)) + min-pycor)
      set-characteristics

    ]
  ]

  create-ex-learners n_init_believers          ;;create single ex-learner believer in closest to center (or random pos?)
   [ ifelse neut-event-YN
     [set color green
      set belief-state 3]
     [set color red
      set belief-state 1]
     ifelse n_init_believers < 2
     [setxy 0 0]
     [setxy ((random (2 * max-pxcor)) + min-pxcor)
           ((random (2 * max-pycor)) + min-pycor)]
     set shape "circle"
     set size 1
     set network-size max_links                ;;idea: (random maxLinks) + 1 *** Could use random-gamma alpha lambda (where alpha is mean/variance, and lambda is 1/(variance/mean))... reflects social media connections (few with lots...most with some)
     set recieve-belief 3
     set com-Num 0
   ]

  setup-priors
  setup-links
  global-initialization
end


to set-characteristics
  set color grey
  set shape "circle"
  set size .5
  set belief-state 0
  set network-size -1
  while [( network-size  < 1) or ( network-size  > 999)][
  set network-size random-normal max_links round(max_links / 2)];(random maxLinks) + 1]
  set recieve-belief 0
  set com-Num 0
  set opinion-threshold 0.5
  set posterior-opinion 0 ;; placeholder for posterior regarding belief
  set SC-sensitivity-bel SC-bel-prop
end


to global-initialization
  set opinion-A-agents count turtles with [color = red]
  set opinion-B-agents count turtles with [color = blue]
  set number-agents count turtles
  set neutral-agents count turtles with [color = grey]
  set tick-tot-odd 1
  set tick-tot-even 0
end



;;##################################################
;; GO
;;##################################################



to go
  ifelse tick-tot-odd = tick-tot-even ;; if there are no more agents getting updated
    [set match-counter match-counter + 1]
    [set match-counter 0]
  if match-counter > 1
    [ stop ]
  ask turtles with [recieve-belief = 1 or recieve-belief = 2]
   [ learn ]
   ask turtles with [belief-state > 0]
   [ propagate ]
  ask turtles
    [
     if belief-state = 1
     [ set clust-num-same count link-neighbors with [color = red]
       set clust-num-diff count link-neighbors with [color = blue]
       if clust-num-same > 0
       [ set clust-prop-same ((clust-num-same / (count link-neighbors)) * 100)]
       if clust-num-diff > 0
       [ set clust-prop-diff ((clust-num-diff / (count link-neighbors)) * 100)]
     ]
     if belief-state = 2
     [ set clust-num-same count link-neighbors with [color = blue]
       set clust-num-diff count link-neighbors with [color = red]
       if clust-num-same > 0
       [ set clust-prop-same ((clust-num-same / (count link-neighbors)) * 100)]
       if clust-num-diff > 0
       [ set clust-prop-diff ((clust-num-diff / (count link-neighbors)) * 100)]
     ]
     ;set cl-prop-same clust-prop-same
     ;report clust-prop-diff
    ]
  set opinion-A-agents count turtles with [color = red]
  set opinion-B-agents count turtles with [color = blue]
  set neutral-agents count turtles with [color = grey]
  ;set b-agent-change UT-agents
  set cl-prop-same mean [clust-prop-same] of turtles
  set cl-prop-diff mean [clust-prop-diff] of turtles
  ;;; Peak Spread
  if (((sqrt ((tick-tot-odd - tick-tot-even) ^ 2)) / n_agents) * 100) > peak-spread
    [set peak-spread (((sqrt ((tick-tot-odd - tick-tot-even) ^ 2)) / n_agents) * 100)]
  ;;;
  ifelse (remainder(ticks + 2) 2) = 1
  [set tick-tot-odd opinion-A-agents + opinion-B-agents]
  [set tick-tot-even opinion-A-agents + opinion-B-agents]
  tick
end



;;##################################################
;; LEARNING AND PROPAGATING
;;##################################################


to learn
 if learning-through-RL = True [                             ;; 1) using RL-model
  set posterior-opinion integration p-H]

  if learning-through-Bayes = true [

    set posterior-opinion BSCm-integration p-H p-E-weighted p-T-weighted recieve-belief
  ]   ;; 2) using BSCM model

  ifelse (random 2 = 1)                                      ;; given enforced coding of >=/=, this prevents one-sided declaration level  ;; evaluation-process resulting in a belief-state congruent with the posterior-opinion
    [ifelse posterior-opinion < 0.5
    [  ;; believers
      set belief-state 2
      set recieve-belief 3
      set color blue
    ]
    [ ;; refuters
      set belief-state 1
      set recieve-belief 3
      set color red
    ]
  ]
  [
    ifelse posterior-opinion <= 0.5
    [  ;; believers
      set belief-state 2
      set recieve-belief 3
      set color blue
    ]
    [ ;; refuters
      set belief-state 1
      set recieve-belief 3
      set color red
    ]
  ]

end


to-report integration [prior]                          ;; function that is used for contrasting the prior-opinion and the received opinion with evidence to arrive at a posterior-opinion

  let evaluation-rounds 1                              ;; setting the first evaluation-round
  let post prior                                       ;; posterior is a now equal to the input to the function, which is prior-opinion (random number between 0 and 1, either drawn from Gaussian or just random float 1)
  let postrials 0                                      ;; variables used for counting how many times evidence that agents sampled was either positive (i.e. opinion A support) or negative (i.e. opinion B support)
  let negtrials 0

  ;;; Reinforcement Learning model of Rescorla and Wagner used for evidence evaluation (equation: Q(t + 1) = Q(t) + βδ(t)) ;;;

  if learning-through-RL = true [
    while[evaluation-rounds < evidence][                ;; sampling as many times as specified in the interface through the evidence slider

    ifelse random 2 = 1                                 ;; randomly determining whether the sampled evidence is going to be positive or negative each round

    [set post post + (learn-parameter * (1 - post))     ;; positive event supportive for opinion A; note that the value in the brackets is the prediction error and that this is based on the RW reinforcement learning model
     set postrials postrials + 1]
    [set post post + (learn-parameter * (0 - post))     ;; negative event supportive for opinion B
     set negtrials negtrials + 1]

    set evaluation-rounds  evaluation-rounds + 1
   ]
  ]
  ;if change-con is true [
   ; abs(prior-opinion - post) > 0.5 [set prior-con (prior-con - (prior-con - abs(prior-opinion - post) / 2))]] ;; potential here to change confidence as well, which might then influence further updating.
  report post
end






to-report BSCm-integration [p-Hyp p-Exp p-Trust flip]   ;; function that is used for contrasting the prior-opinion with evidence to form posterior using modified version of Bayes theorem

let expertise_influence_spec expertise_influence

let p-nH (1 - p-Hyp)     ;; anteriors
let p-nE (1 - p-Exp)
let p-nT (1 - p-Trust)


let p-H-E-T 0.5
let p-H-nE-T 0.5
let p-H-nE-nT 0.5
let p-H-E-nT 0.5

let p-nH-E-T 0.5
let p-nH-nE-T 0.5
let p-nH-nE-nT 0.5
let p-nH-E-nT 0.5


if (flip = 2) [
set p-H-E-T 0.5 - (expertise_influence_spec * 2) ;; specifying CPT
set p-H-nE-T 0.5 - expertise_influence_spec
set p-H-nE-nT 0.5 + expertise_influence_spec
set p-H-E-nT 0.5 + (expertise_influence_spec * 2)

set p-nH-E-T 1 - (0.5 - (expertise_influence_spec * 2))
set p-nH-nE-T 1 - (0.5 - expertise_influence_spec)
set p-nH-nE-nT 1 - (0.5 + expertise_influence_spec)
set p-nH-E-nT 1 - (0.5 + (expertise_influence_spec * 2))

  ]

if (flip = 1) [
set p-H-E-T 0.5 + (expertise_influence_spec * 2) ;; specifying CPT
set p-H-nE-T 0.5 + expertise_influence_spec
set p-H-nE-nT 0.5 - expertise_influence_spec
set p-H-E-nT 0.5 - (expertise_influence_spec * 2)

set p-nH-E-T 1 - (0.5 + (expertise_influence_spec * 2))
set p-nH-nE-T 1 - (0.5 + expertise_influence_spec)
set p-nH-nE-nT 1 - (0.5 - expertise_influence_spec)
set p-nH-E-nT 1 - (0.5 - (expertise_influence_spec * 2))

  ]


;; BAYESIAN SOURCE CREDIBILITY MODEL INTEGRATION


;; Calculating P(Rep|H):
let p-R-H ((p-H-E-T * p-Exp * p-Trust) + (p-H-nE-T * p-nE * p-Trust) + (p-H-nE-nT * p-nE * p-nT) + (p-H-E-nT * p-Exp * p-nT))
;; Calculating P(Rep|¬H):
let p-R-nH ((p-nH-E-T * p-Exp * p-Trust) + (p-nH-nE-T * p-nE * p-Trust) + (p-nH-nE-nT * p-nE * p-nT) + (p-nH-E-nT * p-Exp * p-nT))

;; plugging into BSCm Central Equation to calculate P(H|Rep):
set p-H-R (p-H * p-R-H)/(p-H * p-R-H + p-nH * p-R-nH)


report p-H-R

end


to propagate                  ;; reaching out to other agents and communicating opinion

  if any? link-neighbors with [color = grey] [
    if belief-state = 1 and (random 100 < (prop-likelihood * 100))[
      pick-target (1) (red) (blue) (1)
    ]
    if belief-state = 2 and (random 100 < (prop-likelihood * 100)) [
      pick-target (2) (blue) (red) (-1)
    ]
    ;; neutral starting point - how does a neutral agent communicate

    if belief-state = 3 [
      let com-target link-neighbors with [ color = grey ]
      if com-target != nobody [
        let own-No [who] of self
        let targ-No [who] of com-target
         ask com-target [
          set recieve-belief (random 2 + 1)
          ask link ([who] of self) own-No
          [set color green]
         ]
        ]
    set com-Num com-Num + 1
   ]
  ]
end


to pick-target [ bel-state opin-col-A opin-col-B pos-neg ]
  let com-target link-neighbors with [ color = grey ]
  if com-target != nobody [
    let own-No [who] of self
    let targ-No [who] of com-target
     ask com-target [
      set recieve-belief bel-state



;; counting neighbors congruent what has been communicated and computing their weighted mean P(E) and P(T) values



          if (recieve-belief = 2) [     ;; if they communicated that they believe blue (i.e. zero), here encoded as receive-belief = 2

            set SCval1 count link-neighbors with [color = blue]
            ifelse (SCval1 >= 1)[
             set p-E-mean-blue mean [p-E] of link-neighbors with [color = blue]
             set p-T-mean-blue mean [p-T] of link-neighbors with [color = blue]]
        [set p-E-mean-blue 0.5
             set p-T-mean-blue 0.5]

            set SCval2 count link-neighbors with [color = red]
            ifelse (SCval2 >= 1) [
             set p-E-mean-red mean [p-E] of link-neighbors with [color = red]
             set p-T-mean-red mean [p-T] of link-neighbors with [color = red]]
          [set p-E-mean-red 0.5
             set p-T-mean-red 0.5]

            ;ifelse (SCval1 >= 1 and SCval2 >= 1)[
            set p-E-weighted ((SCval1) / (SCval1 + SCval2))
            set p-T-weighted ((SCval1) / (SCval1 + SCval2))
            if (modulate-weight-by-mean = true)[
             set p-E-weighted ((SCval1 * p-E-mean-blue) / (SCval1 * p-E-mean-blue + SCval2 * p-E-mean-red))
             set p-T-weighted ((SCval1 * p-T-mean-blue) / (SCval1 * p-T-mean-blue + SCval2 * p-T-mean-red))] ]
       ; [
        ;  set p-E-weighted 0.5
         ; set p-T-weighted 0.5

       if (recieve-belief = 1) [
            set SCval1 count link-neighbors with [color = red]    ;; if they communicated that they believe red (i.e. 1), here encoded as receive-belief = 1
            ifelse (SCval1 >= 1)[
             set p-E-mean-red mean [p-E] of link-neighbors with [color = red]
          set p-T-mean-red mean [p-T] of link-neighbors with [color = red]]
          [set p-E-mean-red 0.5
             set p-T-mean-red 0.5]

            set SCval2 count link-neighbors with [color = blue]
            ifelse (SCval2 >= 1) [
             set p-E-mean-blue mean [p-E] of link-neighbors with [color = blue]
             set p-T-mean-blue mean [p-T] of link-neighbors with [color = blue]]
          [set p-E-mean-blue 0.5
             set p-T-mean-blue 0.5]

          ;  ifelse (SCval1 >= 1 and SCval2 >= 1)[
            set p-E-weighted ((SCval1) / (SCval1 + SCval2))
            set p-T-weighted ((SCval1) / (SCval1 + SCval2))
            if (modulate-weight-by-mean = true)[
             set p-E-weighted ((SCval1 * p-E-mean-red) / (SCval1 * p-E-mean-red + SCval2 * p-E-mean-blue))
             set p-T-weighted ((SCval1 * p-T-mean-red) / (SCval1 * p-T-mean-red + SCval2 * p-T-mean-blue))]]
        ;[
          ;set p-E-weighted 0.5
          ;set p-T-weighted 0.5


      ;]

      ;set communication-direction recieve-belief      ;; communication direction takes teh value of the received belief state and is then used in the BMSC function to dictate flipping behaviour
      ;set p-H p-H ;(p-H + (pos-neg * p-h-given-c))    ;; thats the question now - how do we want to deal with the impact of communication? leave it here or put it somewhere else

      if (no-social-influence = true)[
      set p-E-weighted random-float 1
        set p-T-weighted random-float 1]
      ask link ([who] of self) own-No
      [set color opin-col-A]
   ]

  ]

end



;;##################################################
;; REPORTERS
;;##################################################

to-report sample-beta [ alpha beta ]               ;; generic function that creates a beta distribution based on two gamma distributions
  let Xa random-gamma alpha 1                      ;; the scaling parameter is set to 1 for both gammas (see https://math.stackexchange.com/questions/190670/how-exactly-are-the-beta-and-gamma-distributions-related)
  let Xb random-gamma beta 1
  let X Xa / ( Xa + Xb)
  report X                                         ;; reporting a variable drawn from the beta distribution
end



;;##################################################
;; FUNCTIONS SOURCED FROM OTHER MODELS
;;##################################################

;;## 1 ## Wilensky, U. (2005).  NetLogo Preferential Attachment model.  http://ccl.northwestern.edu/netlogo/models/PreferentialAttachment. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

to make-node [old-node]
  create-turtles 1
  [
    set color red
    if old-node != nobody
      [ create-link-with old-node [ set color green ]
        ;; position the new node near its partner
        move-to old-node
        fd 8
      ]
  ]
end

;; This code is the heart of the "preferential attachment" mechanism
to-report find-partner
  report [one-of both-ends] of one-of links
end

;;;;;;;;;;;;;;
;;; Layout ;;;
;;;;;;;;;;;;;;

;; resize-nodes, change back and forth from size based on degree to a size of 1
to resize-nodes
  ifelse all? turtles [size <= 1]
  [
    ;; a node is a circle with diameter determined by
    ;; the SIZE variable; using SQRT makes the circle's
    ;; area proportional to its degree
    ask turtles [ set size sqrt count link-neighbors ]
  ]
  [
    ask turtles [ set size 1 ]
  ]
end

to layout
  ;; the number 3 here is arbitrary; more repetitions slows down the
  ;; model, but too few gives poor layouts
  repeat 3 [
    ;; Refactoring the link lengths (MODIFY DENSITY HERE?)
    ;; the more turtles we have to fit into the same amount of space,
    ;; the smaller the inputs to layout-spring we'll need to use
    let factor ((sqrt count turtles) / 3) ;; Here SF-density-mod influences the distance factor across the network - will impact search function...
    ;; numbers here are arbitrarily chosen for pleasing appearance
    layout-spring turtles links (1 / factor) (7 / factor) (1 / factor)
    display  ;; for smooth animation
  ]
  ;;; Centering network ;;;
  ;; don't bump the edges of the world
  let x-offset max [xcor] of turtles + min [xcor] of turtles
  let y-offset max [ycor] of turtles + min [ycor] of turtles
  ;; big jumps look funny, so only adjust a little each time
  set x-offset limit-magnitude x-offset 0.1
  set y-offset limit-magnitude y-offset 0.1
  ask turtles [ setxy (xcor - x-offset / 2) (ycor - y-offset / 2) ]
end

to-report limit-magnitude [number limit]
  if number > limit [ report limit ]
  if number < (- limit) [ report (- limit) ]
  report number
end


;;#####################################
@#$#@#$#@
GRAPHICS-WINDOW
268
10
719
462
-1
-1
3.6612
1
1
1
1
1
0
0
0
1
-60
60
-60
60
0
0
1
ticks
30.0

BUTTON
684
493
739
562
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
741
493
796
562
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
7
65
122
98
n_agents
n_agents
0
10000
1000.0
1
1
NIL
HORIZONTAL

PLOT
771
59
931
209
P(H)
NIL
NIL
0.0
1.0
0.0
100.0
true
false
"" "set-plot-y-range 0 round(n_agents / 20)"
PENS
"P(H)" 0.01 1 -16777216 true "" "histogram [p-H] of turtles"

PLOT
935
57
1326
207
P(T), P(E), prior-con
NIL
NIL
0.0
1.0
0.0
2.0
true
true
"" "set-plot-y-range 0 round(n_agents / 20)"
PENS
"p-expertise" 0.01 1 -7858858 true "" "histogram [p-E] of turtles"
"p-trust" 0.01 1 -13840069 true "" "histogram [p-T] of turtles"

SLIDER
1378
140
1478
173
min_con
min_con
0
1
0.66
0.01
1
NIL
HORIZONTAL

SLIDER
1377
178
1478
211
max_con
max_con
0
1
0.0
0.01
1
NIL
HORIZONTAL

TEXTBOX
708
472
786
490
Setup and Go
11
0.0
1

TEXTBOX
47
21
197
39
Agent-specific settings
11
0.0
1

SLIDER
1378
216
1480
249
alpha_con
alpha_con
0
10
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
1378
252
1479
285
beta_con
beta_con
0
10
0.0
0.01
1
NIL
HORIZONTAL

MONITOR
1176
210
1248
255
con_mean
(min_con * beta_con + max_con * alpha_con) / (alpha_con + beta_con)
3
1
11

MONITOR
1250
210
1326
255
con_sd
sqrt(((alpha_con * beta_con * (max_con - min_con)^(2)))/(((alpha_con + beta_con)^(2)) * (1 + alpha_con + beta_con)))
3
1
11

SWITCH
6
132
122
165
scale-free
scale-free
1
1
-1000

TEXTBOX
33
53
183
71
number of agents 
8
0.0
1

TEXTBOX
22
111
115
137
scale free vs. random network
8
0.0
1

SLIDER
144
138
245
171
evidence
evidence
0
100
0.0
1
1
NIL
HORIZONTAL

TEXTBOX
1003
42
1179
68
NIL
10
0.0
1

TEXTBOX
1366
294
1520
320
confidence parameters
9
0.0
1

TEXTBOX
146
114
281
156
amount of evidence an agent samples to evaluate opinion
8
0.0
1

SLIDER
148
67
249
100
max_links
max_links
0
500
17.0
1
1
NIL
HORIZONTAL

TEXTBOX
151
43
275
69
maximum number of links an agent possesses
8
0.0
1

SLIDER
766
214
858
247
prior-mean
prior-mean
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
862
214
954
247
prior-sd
prior-sd
0
1
0.2
0.01
1
NIL
HORIZONTAL

SWITCH
7
382
119
415
neut-event-YN
neut-event-YN
0
1
-1000

SLIDER
6
323
123
356
n_init_believers
n_init_believers
0
100
1.0
1
1
NIL
HORIZONTAL

SWITCH
6
198
121
231
prox-YN
prox-YN
0
1
-1000

SLIDER
145
314
245
347
p-h-given-c
p-h-given-c
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
144
268
243
301
prop-likelihood
prop-likelihood
0
1
0.1
0.01
1
NIL
HORIZONTAL

SWITCH
7
263
120
296
nb-prop-YN
nb-prop-YN
1
1
-1000

SLIDER
144
203
248
236
sc-bel-prop
sc-bel-prop
0
5
0.6
0.01
1
NIL
HORIZONTAL

TEXTBOX
163
187
269
213
conformity
8
0.0
1

SWITCH
815
637
986
670
con-in-prior
con-in-prior
1
1
-1000

TEXTBOX
838
613
969
652
if no, agents have no confidence in their prior opinion
8
0.0
1

PLOT
9
471
209
621
Propagation rate
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ((sqrt ((tick-tot-odd - tick-tot-even) ^ 2)) / n_agents) * 100"

PLOT
222
471
423
621
Proportion Time Plot
Time
Proportion
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Opinion A" 1.0 0 -2674135 true "" "plot opinion-A-agents "
"Opinion B" 1.0 0 -13345367 true "" "plot opinion-B-agents"

TEXTBOX
30
371
180
389
first event neutral?
8
0.0
1

TEXTBOX
34
174
130
200
close links vs. links everywhere
8
0.0
1

TEXTBOX
25
242
115
268
can non-opinion holders communicate?
8
0.0
1

TEXTBOX
48
302
114
328
number of initial opinion holders 
8
0.0
1

TEXTBOX
155
246
305
266
likelihood that opinion is declared/propagated
8
0.0
1

TEXTBOX
1629
16
1779
34
Parameters\n
11
0.0
1

PLOT
432
471
632
621
Degree of Clustering Post-Cascade
Time
Percentage of Neighbours 
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Similar " 1.0 0 -16777216 true "" "plot cl-prop-same"
"Different" 1.0 0 -7500403 true "" "plot cl-prop-diff"

SWITCH
1019
636
1175
669
con-in-learning
con-in-learning
1
1
-1000

TEXTBOX
1030
611
1180
631
if no, agents have a homogeneous learning parameter
8
0.0
1

SWITCH
850
453
1037
486
learning-through-RL
learning-through-RL
1
1
-1000

TEXTBOX
1060
413
1210
431
Updating mechanism
11
0.0
1

TEXTBOX
865
428
1015
456
learning through Rescorla-Wagner RL model
8
0.0
1

SWITCH
1194
635
1383
668
quadratic-con-prior?
quadratic-con-prior?
1
1
-1000

TEXTBOX
1218
610
1368
630
if the relationship with confidence is quadratic
8
0.0
1

SWITCH
1052
452
1258
485
learning-through-Bayes
learning-through-Bayes
0
1
-1000

TEXTBOX
1088
433
1238
461
learning through Bayes Theorem
8
0.0
1

SLIDER
2105
321
2197
354
min_E
min_E
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
2105
358
2201
391
max_E
max_E
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
2105
400
2205
433
alpha_E
alpha_E
0
10
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
2105
435
2204
468
beta_E
beta_E
0
10
0.0
0.01
1
NIL
HORIZONTAL

TEXTBOX
2110
478
2260
500
expertise parameters
9
0.0
1

SLIDER
2206
320
2298
353
min_T
min_T
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
2206
357
2298
390
max_T
max_T
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
2208
393
2300
426
alpha_T
alpha_T
0
10
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
2209
430
2301
463
beta_T
beta_T
0
10
0.0
0.01
1
NIL
HORIZONTAL

TEXTBOX
2217
477
2367
499
trustworthiness parameters
9
0.0
1

MONITOR
1251
258
1326
303
p-E_sd
sqrt(((alpha_E * beta_E * (max_E - min_E)^(2)))/(((alpha_E + beta_E)^(2)) * (1 + alpha_E + beta_E)))
2
1
11

MONITOR
1252
306
1327
351
p-T_sd
sqrt(((alpha_T * beta_T * (max_T - min_T)^(2)))/(((alpha_T + beta_T)^(2)) * (1 + alpha_T + beta_T)))
2
1
11

MONITOR
1176
258
1248
303
p-E_mean
(min_E * beta_E + max_E * alpha_E) / (alpha_E + beta_E)
2
1
11

MONITOR
1177
306
1250
351
p-T_mean
(min_T * beta_T + max_T * alpha_T) / (alpha_T + beta_T)
2
1
11

TEXTBOX
951
24
1101
42
Descriptives
11
0.0
1

SWITCH
2311
436
2451
469
change_shape?
change_shape?
1
1
-1000

TEXTBOX
2311
375
2461
431
If change_shape? is off, values will be sampled from normal distribution. Please specify mean and sd below:
11
0.0
1

SLIDER
1494
179
1597
212
p-E-mean
p-E-mean
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
1603
177
1709
210
p-E-sd
p-E-sd
0
1
0.2
0.01
1
NIL
HORIZONTAL

SLIDER
1494
216
1600
249
p-T-mean
p-T-mean
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
1603
218
1710
251
p-T-sd
p-T-sd
0
1
0.2
0.01
1
NIL
HORIZONTAL

SLIDER
1494
253
1601
286
p-con-mean
p-con-mean
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
1604
254
1711
287
p-con-sd
p-con-sd
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
849
491
1037
524
learning_rate
learning_rate
0
1
0.0
0.01
1
NIL
HORIZONTAL

TEXTBOX
1662
336
1812
364
CPT for the source credibility model
11
0.0
1

TEXTBOX
1451
423
1505
441
H-true
11
0.0
1

TEXTBOX
1452
464
1507
482
H-n-true
11
0.0
1

TEXTBOX
1535
385
1562
403
T-E
11
0.0
1

TEXTBOX
1634
384
1674
402
T-nE
11
0.0
1

TEXTBOX
1731
383
1774
401
nT-E
11
0.0
1

TEXTBOX
1824
383
1882
401
nT-nE
11
0.0
1

TEXTBOX
934
574
1236
630
If using RL-model, confidence can be incorporated to influence updating behaviour
11
0.0
1

MONITOR
1525
403
1583
440
p-H-E-T
0.5 + (expertise_influence  * 2)
2
1
9

TEXTBOX
1630
530
1780
563
determines the magnitude to which expertise influences opinion formation
9
0.0
1

MONITOR
1525
447
1584
484
p-nH-T-E
1 - (0.5 + (2 * expertise_influence))
2
1
9

MONITOR
1619
403
1679
440
p-H-T-nE
0.5 + expertise_influence
2
1
9

MONITOR
1617
444
1680
481
p-nH-T-nE
1 - (0.5 + expertise_influence)
2
1
9

MONITOR
1716
403
1781
440
p-H-nT-E
0.5 - (expertise_influence * 2)
2
1
9

MONITOR
1716
445
1782
482
p-nH-nT-E
1 - (0.5 - (expertise_influence * 2))
2
1
9

MONITOR
1805
403
1877
440
p-H-nT-nE
0.5 - expertise_influence
2
1
9

MONITOR
1805
445
1877
482
p-nH-nT-nE
1 - (0.5 - expertise_influence)
2
1
9

SWITCH
1051
490
1258
523
con-in-prior
con-in-prior
1
1
-1000

TEXTBOX
2138
261
2288
303
Please ignore theparameters below for now
11
0.0
1

SLIDER
141
379
233
412
ground-truth
ground-truth
0
1
0.0
0.01
1
NIL
HORIZONTAL

SWITCH
1051
527
1258
560
ground-truth-impact?
ground-truth-impact?
1
1
-1000

PLOT
769
259
941
395
 P(H|rep)
NIL
NIL
0.0
1.0
0.0
100.0
false
false
"" ";set-plot-y-range 0 round(n_agents / 20)"
PENS
"default" 0.01 1 -16777216 true "" "histogram [posterior-opinion] of turtles"

SWITCH
1263
453
1417
486
prior-variance
prior-variance
0
1
-1000

TEXTBOX
1270
414
1420
456
if prior-variance is turned off, all agents start with P(E), P(H), and P(T) = .5
9
0.0
1

SLIDER
1263
492
1419
525
con-threshold
con-threshold
0
1
0.0
0.01
1
NIL
HORIZONTAL

PLOT
954
262
1136
391
p-E-weighted
NIL
NIL
0.0
1.1
1.0
100.0
true
false
"" ""
PENS
"default" 0.01 1 -16777216 true "" "histogram [p-E-weighted] of turtles"

SWITCH
1146
359
1370
392
modulate-weight-by-mean
modulate-weight-by-mean
0
1
-1000

SLIDER
1614
492
1842
525
expertise_influence
expertise_influence
0
1
0.2
0.01
1
NIL
HORIZONTAL

SWITCH
1263
528
1421
561
no-social-influence
no-social-influence
1
1
-1000

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Replication_2017" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <metric>cl-prop-same</metric>
    <metric>ticks</metric>
    <metric>peak-spread</metric>
    <steppedValueSet variable="max_links" first="1" step="10" last="500"/>
    <enumeratedValueSet variable="min_con">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.58"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gaussian-prior">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <metric>cl-prop-same</metric>
    <metric>ticks</metric>
    <metric>peak-spread</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="1900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="4.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="3.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.49"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0.42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="-0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-conformity">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="169"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.49"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="-0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-conformity">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="169"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.49"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="-0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="116"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="-0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="134"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="29" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="134"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="-0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="134"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="-0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="134"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="-0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="29"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="-0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <metric>cl-prop-same</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="-0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="200" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="500" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="500" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="500" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="-0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="23"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="0.13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="700"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="700"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <metric>cl-prop-same</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="500" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>opinion-A-agents</metric>
    <metric>opinion-B-agents</metric>
    <enumeratedValueSet variable="max_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_links">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_con">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_init_believers">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-learning">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quadratic-con-prior?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth">
      <value value="0.69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con-in-prior">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-h-given-c">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_agents">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-Bayes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ground-truth-impact?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-sd">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-through-RL">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expertise_influence">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior-variance">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-free">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="change_shape?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-sd">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-con-mean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sc-bel-prop">
      <value value="0.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_con">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-E-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modulate-weight-by-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning_rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neut-event-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prox-YN">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-prop-YN">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_con">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-T-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_con">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta_T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evidence">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_E">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_T">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
