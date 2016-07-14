;; See: https://www.reddit.com/r/dailyprogrammer/comments/3ofsyb/20151012_challenge_236_easy_random_bag_system/cvye117

random/seed now

rchars: does [ random "OISZLJT" ]

res: copy/part join "" loop 8 [ append/only [] copy rchars ] 50
print [ "Output: " res ]

test: copy/part res 50 - 2
if err: forall test [
   if (equal? first test second test) and (equal? second test third test )
      [ break/return test ]
] [
  print [ "Triple at: " err ]
]

;; Run as: ./r3 challenge_236.r
;; Output (sample): Output:  STZOJILTOJSZILTOILJSZLJSZTOITISJOLZISZLJTOISOTZLJL
