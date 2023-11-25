- [x] i
    - [x] a
        - function was simple enough. for fun, added a padding function.
    - [x] b
        - first was edge detection
- [ ] ii
    - [ ] a
        - 7 layers:
            - 4 Conv2D layers, all 3x3 kernel size, all 16 output filters
            - 1 Dropout layer w/ a seed of 0.5
            - 1 Flatten layer
            - 1 Dense layer
    - [ ] b
        - [ ] i 
            - 37146 parameters
            - Most parameters in Dense (see notes 8.2 pg8)
            - Test data is less accurate than training data
            - Most common has 10% accuracy
        - [x] ii
            - Matches general curve, so not under-fitting. Is still increasing by 20 epochs, so not over-fitting.
        - [ ] iii
            - 5k: 49%, ~0.7s/epoch, 14s total
            - 10k: 57%, ~1.1s/epoch, 22s total
            - 20k: 62%, ~2.45s/epoch, 49s total
            - 40k: 67%, ~4.7s/epoch, 94s total
            - The prediction accuracy increases with more data. The two plots grow closer together the more data is trained. This implies that, given more data, the accuracy will eventually begin to over-fit the data and match the training data.
        - [ ] iv
            - 0: 0.48, no over-fitting
            - 0.0000001: 0.51, no over-fitting
            - 0.0001: 0.49, no over-fitting
            - 0.001: 0.47, no over- or under-fitting
            - 0.01: 0.42, potential under-fitting
            - 0.1: 0.34, under-fitting
            - 1: 0.14, overt under-fitting
            - Lowering the weight increases accuracy up to a point. The smaller the weight, the more accurate it is with little over-fitting, but if it gets to 0, it drops. On the other hand, the higher the amount of training data, the more accurate the predictions (moreso even than decreasing the weight), but it shows a potential to over-fit at some point in the future.

            Similarly, the lower the amount of data, the closer it will get to a random baseline classifier; the same can be said for lowering the weight. However, since increasing/decreasing the amount of training data depends on the amount of actual data in the dataset, whereas changing the weight can be done freely, I would say changing the weight is more affective at managing over-fitting. Although I would still recommend using a mix of both strategies in real applications, as both options have their strengths.
    - [ ] c
        - [ ] i
            - just change the code
        - [ ] ii
            - 