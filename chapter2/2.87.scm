#| 假设没有（2 ‘（））这种项存在，但是可能存在（2 0）这种 |#
   (define (=zero-poly? poly)
        (apply and (map =zero? (map coeff (term-list poly)))))

#| 好强，我指我自己 |#
