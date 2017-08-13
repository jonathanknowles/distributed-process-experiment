# Distributed Process Experiment

A small experiment with the Haskell `distributed-process` library.

## Contents

* [Introduction](#introduction)
* [Building](#building)
* [Installation](#installation)
* [Usage](#usage)
* [Design](#design)
* [Performance](#performance)

## Introduction

This experiment consists of a *master node* and a number of *slave nodes* that continually send pseudo-random messages to each another for a configurable period of time.

### Slave nodes

Each slave node:

* generates a continuous stream of timestamped random messages;
* sends every message to each of the other slave nodes;
* makes a best effort to receive all messages from each of the other slaves;
* sorts received messages into order, according to their respective timestamps;
* continually maintains a digest of all received messages from other slaves.

When the experiment is over, each slave reports its final digest.

### Master node

The master node:

* uses UDP multicast to detect the existence of slave nodes;
* gives each slave node knowledge of each of the other slave nodes;
* gives each slave node a unique starting seed with which to generate random numbers.

### Messages

Each message consists of a pseudo-random real number in the range `0` to `1`. A timestamp is attached at the time of sending. Pseudo-random numbers are generated according to a seed supplied by the master.

## Building

Issue the following commands:
```
stack setup
stack build
```

Issue the following command to run the automated test suite:
```
stack test
```

## Installation

Issuing the following command will install the `distributed-process-experiment` executable file into your `~/.local/bin` directory:
```
stack install
```

## Usage

### Slave nodes

Slave nodes require very little configuration. To start a slave node, issue the following command:
```
distributed-process-experiment slave --host HOSTNAME --port INT
```
The slave node will bind to the specified *host name* and *port* and wait for further instructions.

It's important that every slave node has a unique (*host name*, *port*) pair, and that the host name used to initialize the slave node can be resolved by peer nodes.

### Master node

Note: the master node discovers slave nodes during startup. Before starting a master node, first ensure that all slave nodes have been started.

To start a master node in the simplest way possible, issue the following command:
```
distributed-process-experiment master \
    --host HOSTNAME --port INT        \
    --send-for INT --wait-for INT
```
The master node will bind to the specified *host name* and *port* and then attempt to discover slave nodes. Once slave nodes have been discovered, the master will issue each slave node with a unique seed and a list of all the other slave nodes, at which point each of the slave nodes will start sending messages.

The `--send-for` parameter specifies a *sending period*: for how long (in seconds) each slave should send messages.

The `--wait-for` parameter specifies a *waiting period*: for how long (in seconds) each slave should continue to receive and process messages after the sending period is over.

After the sending and waiting periods are over, the master will terminate each of the slave nodes.

#### Specifying a seed

By default, the master node will use a starting seed value of `0` to deterministically generate unique seeds for each of the slaves. To specify a *different* starting seed, use the  `--with-seed` parameter:
```
distributed-process-experiment master \
    --host HOSTNAME --port INT        \
    --send-for INT --wait-for INT     \
    --with-seed INT
```

#### Specifying a block size

By default, slave nodes accumulate multiple messages into *blocks* before sending them to other slaves. To specify that slaves should use a particular *block size*, use the `--block-size` parameter:
```
distributed-process-experiment master \
    --host HOSTNAME --port INT        \
    --send-for INT --wait-for INT     \
    --block-size INT
```

Specifying a block size of `n` will cause slave nodes to send messages in blocks of `n` messages.

The *default* block size is 1024 messages.

## Design

This implementation makes several design choices.

### Message bundling

By default, slaves bundle messages together into *blocks* before transmitting them over the network to other slaves.

This has several advantages:

* Messages can be *generated in batches*, allowing for more efficient use of the processor and memory. Assuming the compiler is able to transform message generation code into a tight loop, and assuming consecutive messages can be written to contiguous chunks of memory, then it make sense to generate several messages at once, taking advantage of the principles of spatial and temporal locality.

* Messages can be *consumed in batches*, again allowing for more efficient use of the processor and memory. Assuming the compiler is able to transform message digest code into a tight loop, and assuming consecutive messages can be read from contiguous chunks of memory, then it make sense to digest several messages at once, taking advantage of the principles of spatial and temporal locality.

* Received messages can be *sorted more efficiently* into timestamp order. Since large numbers of messages are effectively sent *at the same time*, they can share a timestamp. For a stream of *n* messages, the time complexity of sorting messages into order is as follows:

    | Scenario                                                                                                          | Time complexity of sorting messages |
    |-------------------------------------------------------------------------------------------------------------------|-------------------------------------|
    | • _n_ messages are sent individually              <br> • each message has a unique timestamp                      | O (_n_ log _n_)                     |
    | • _n_ messages are sent within blocks of size _k_ <br> • messages within the same block share a common timestamp  | O (_n_/_k_ log _n_/_k_)             |

* Sending a message over the network comes with an associated *overhead*. Bundling messages together allows this overhead to be spread over many messages, allowing for a lower amortised network overhead.

### Work stealing

In order to achieve a *bounded memory footprint*, slaves wait for *permission* from other slaves before sending blocks.

This has the following advantages:

* Assuming some slaves are capable of generating messages at a faster rate than other slaves can consume them, this helps avoid the situation where slower slaves suffer from memory exhaustion.

* In the case of network disruption, slaves do not continue to build up a queue of untransmitted messages, thus avoiding memory exhaustion and lowering the likelihood of lost messages.

In practice, after broadcasting any given block, a slave will wait for *all other slaves* to produce a block before digesting those blocks and broadcasting another block. This has the disadvantage that slaves only move forward in lockstep with one another (and thus some slaves are left idle when they could be doing work). However, this disadvantage is offset by the efficiency gained from generating (and digesting) messages in batches.

### Digest equality

Every slave digests messages in exactly the same order. If slaves *s1* and *s2* respectively transmit messages *m1* and *m2* at times *t1* and *t2*, then all slaves will digest message *m1* before digesting message *m2*. Similarly, if slaves *s1* and *s2* respectively transmit message blocks *b1* and *b2* at times *t1* and *t2*, then all slaves will digest block *b1* before digesting block *b2*.

This has the effect that, if all slaves receive and digest all transmitted messages, then they will all ultimately compute exactly the same digest value.

Note this is not a guarantee that all messages will be received and digested. Over a given run with a set of slaves *S* and a timestamp-ordered list of messages *M*, some slaves might only receive a prefix of *M*. However, prefixes of the *same length* will produce *identical* digest values.

## Performance

### Effect of message block size

Varying the message block size can have a marked effect on overall message transmission rate.

As the number of messages in a block increases, the overall message transmission rate tends to increase, eventually hitting an upper bound (the message transmission rate can never exceed the message generation rate):

![graph](http://jonathanknowles.net/distributed-process-experiment/data/message-block-size-vs-message-receive-rate.svg)

The above graph was produced from data obtained with the following configuration:

#### Hardware

| Host |         CPU         | Cores | Frequency | Memory | Frequency |
|:----:|:-------------------:|:-----:|:---------:|:------:|-----------|
|   1  | Intel Core i7-3537U |   4   |  2.00 GHz |  4 GiB | 1600 MHz  |
|   2  |   AMD Athlon 750K   |   4   |  1.40 GHz |  8 GiB | 1600 MHz  |

#### Nodes

| Host | Master Node   | Slave Nodes |
|:----:|:-------------:|:-----------:|
|   1  | X             |   4         |
|   2  |               |   4         |

#### Parameters

| Sending Period | Waiting Period |
|---------------:|---------------:|
|           60 s |           10 s |

