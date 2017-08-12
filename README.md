# Distributed Process Experiment

A small experiment with the Haskell `distributed-process` library.

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

### The master node

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

## Running

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

## Graphs

![graph](http://jonathanknowles.net/distributed-process-experiment/data/message-block-size-vs-message-receive-rate.svg)

