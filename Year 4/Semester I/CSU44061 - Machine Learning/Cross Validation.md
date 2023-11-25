While evaluating the cost function on training data can be sufficient, we're really interested in how well the modal makes prediction for new data, or how well it **generalises**.

We can split the training data into 
- test data used to evaluated prediction performance, and
- training data used to train the model

There is a trade-off in how the data is split. If we use more for training then it will adapt to new data better but will have less data to test itself on. Conversely, if more is used for testing to see how well it does with larger quantities of data, there will be less to train the model on. Usually, the training/testing data is split 80/20 or 90/10.

## Hold-out Method
![[hold_out_method.png]]

(code: [[Sample Code#^hold-out-method|Hold-out Method]])

## $k$-Fold Cross-Validation
Repeatedly applying the hold-out method (above) using random splits is fine, but it's more common to use $k$-fold cross validation. This is done by:
- Divide the data into $k$ equal-sized parts
- Use part 1 as test data and the rest as training data, then calculate $J(\theta)$ for that part
- Use part 2 as test data and the rest as training data, etc.
This gives us $k$ estimates of $J(\theta)$ which we can use to estimate the average and spread of values.

![[k-fold_cross-validation.png]]

(code: [[Sample Code#^k-fold-cross-validation|k-fold-cross-validation]])
