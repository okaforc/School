(8.3)
## Why Convolutions?
**Sparsity:** Local information is often enough to detect basic features such as edges. There's no need to look at the entire image.
**Reuse:** A feature extractor that works on one part of an image likely works on other parts, so it can be reused.

As a result of this, there can be a massive reduction in number of parameters to learn. For example, a 3x3x3 kernel has $3^{3}=27$ parameters, but with a separate kernel for every part of a 300x300 image, there would need to be $298\times298 \approx 89K$ kernels.

Sparsity is a useful feature, but for larger features (e.g., a building) we want a kernel that acts directly on the input image. 

The *receptive field* is the kernel size. 

The receptive field of a kernel in the second layer is $3\times3$ kernels from the first; that is $9\times9$ pixels from the image. The receptive field of the third layer is therefore $3\times3$ kernels from the second, i.e. $27\times27$ pixels from the image.

The first layer in a convolutional network (ConvNet, convnet) "sees" local image features. Deeper layers combine lesser layers and can "see" larger features in an image, e.g. can combine multiple lines/edges to recognise an eye. This is the intuition behind using multiple layers in ConvNet to form *deep learning*.

### Typical Modern Convolutional Neural Network (CNN Structure)
As we go through the CNN, the number of channels increases and the height and width decrease (a general rule of thumb is to repeatedly halve the height and width while doubling the number of channels).

A common pattern is $$(\textrm{Conv} \times n +\textrm{Maxpool} \times m)$$
where:
- $n$ convolutional layers with the same padding, followed by a max-pool layer to down-sample
- repeat this block $m$ times
- often have $3\times3$ kernels in conv layers and $(2, 2)$ max-pooling
The final or output layer is a full-connected layer, often softmax. We can think of this setup as convolutional layers being used to generate features that are then used as input to a logistic regression classifier (i.e., the softmax layer).

### LeNet
LeNet was designed for MNIST digit dataset. It's rather old and it has outdates/now-unpopular methods such as average pooling and sigmoid and $tanh$ activation. It also uses a relatively large $5\times5$ kernels but is quite small, with a network of only about 60K parameters.