Dimple2
=======

An implementation Dimple-II membership protocol for epidemic protocols
with support for automatic discovery. Dimple2 is an library designed
to be used from within a Haskell program. However, the Dimple2 peers
interact with one another through a REST api and that REST api has
been extended to support all Dimple2 interactions.

The membership protocol is full discussed in the DIMPLE-II: Dynamic
Membership Protocol for Epidemic Protocols [^dimpleii].  document in
the. The exception to this is the discovery protocol which is
independent of DIMPLE-II and fully discussed here.

Introduction
------------

The engineering approach used by DIMPLE-II is based on a set of
distributed algorithms, for which each node (1) allocates a tiny
subset of the entire membership at each node with the goal that each
node has a different subset (2) exchanges part of the tiny subset with
randomly chosen peers in its subset (3) performs statistical analysis
on the tiny subset to estimate the entire dynamic membership size.

The essence of this approach is to use two concepts: shuffling and
sampling. These shuffling and sampling routines are used to maintain a
*Local View* of membership in the network.

Local View
----------

 The Local View is the current nodes view on the network. It contains
all of the peer nodes that the current node is aware of and serves as
the basis for the rest of the algorithms in the system.

Each entry in the local view consists of three pieces of
information.

* The URL of the node itself
* The age of the URL on the current node represented as a non negative integer
* A visited list of nodes that this URL has passed through before
  arriving at the current node. The visited list size is bounded by
  the shortest average path length of the network.

The size of the Local View is `O(log(N))` where `N` is the estimated
size of the network[^estimate].

Shuffle
-------

1. Increase the age of all nodes in the Local View by 1
2. Repeat Shuffle-Initiate half the size of the Local View times

### Shuffle Initiate


1. Select the oldest node (by the age field) 'SELECTED' in the local
   view of the current node 'CURRENT'.
2. Set the age of that SELECTED to zero in the Local View
3. Send the URL of 'CURRENT' to 'SELECTED'
4. When CURRENT receives a response from SELECTED, CURRENT adds the
   node received from SELECTED in an empty slot or if there is no open
   slot, then the slot used by SELECTED is used instead and SELECTED
   is replaced in the Local View
5. If no response is received from SELECTED or SELECTED is
   uncontactable then SELECTED is removed from the Local View.

### Shuffle Response

*When SELECTED's Local View has open slots:*

1. SELECTED randomly selects a value node from it's current Local View
2. SELECTED adds CURRENT's information to SELECTED's Local View.
3. SELECTED returns the randomly selected value node to CURRENT.

*When SELECTED's Local View has no open slots:*

1. SELECTED randomly selects a value node in it's current Local View.
2. Replaces that value node with CURRENT's information.
3. Returns the value node information to CURRENT.

Join
----

1. CURRENT contacts a node selected as an introducer (INTRODUCER)
  * The introducer can be a randomly selected node or a node
    introduced by some outside party.
2. On receiving the request, INTRODUCER builds a Local View by
   collecting the oldest node in the visited list of each entry in
   INTRODUCER's Local View
3. INTRODUCER return this new Local View to CURRENT
4. CURRENT uses this new Local View as its Local View

Network Size Estimation
-----------------------

On initialization the current node creates two sampling buffers (S1
and S2). S1 is used for the first sampling and S2 is used for the
second sampling.

At every cycle the node adds half the Local View to S1 and the
other half to S2. The first and second halves are chosen randomly.

The two buffers are used in FIFO fashion, the oldest samplings is
dropped when the buffer overflows.

   N = (NS1 x NS2) / D

Where the `N` is the current network size, `NS1` is the current size of
`S1`, `NS2` is the current size of `S2` and `D` is the number of duplicates
between `S1` and `S2`.


Automatic Discovery -------------------

When the Dimple2 system is started it sends out a multicast broadcast
containing the URL in
[Netstrings](http://cr.yp.to/proto/netstrings.txt) format. Peers that
hear that broadcast should optionally `POST` their URL to the
`interested` peer API. Regardless of the number of nodes that hear
this broadcast no more then about 10 - 100 nodes should respond. This
implementation uses a probabilist algorithm based on the estimated
number of nodes in the system to ensure that property.

### Algorithm

0. Check to see if there are any peers in the `localview`
1. If peers exist wait N seconds and try again
2. If no peers exist, Broadcast Peer URL in Netstrings format
4. Collect responses from interested parties for N seconds
5. If interested parties have responded, start join operation
6. If interested parties have not responded start broadcast process again

Manual Discovery
----------------

To manually force discovery you may put a url end point to the local
view. This will have a high probability of forcing a shuffle to occur.

REST API
--------

*All json descriptions are in the format of js-schema*. They should be
readable on their own, but more information can be found at the
[js-schema](https://github.com/molnarg/js-schema/) site.

### Types used throughout

    var URL = String;

    var Peer = schema ({url : URL,             // The url of the peer
                        age : Number.min(0)}); // the number of failed
                                               // sync attemps

     var Interested = URL;                     // url of the node interested
                                               // in communicating

### Interested Nodes Collection

    /dimple2/interested
      GET - Get the list of interested nodes
      POST - Post a new nod to the list of interested nodes

#### `POST`


This api exists to allow for automated discovery. Peers interested in
participating are expected to `POST` a value represented by the
`Interested` scheme to `/dimple2/interested`.

#### `GET`

     Array.of(Interested)

This api returns a list of urls that are currently in the `interested`
list of this target Peer. *NOTE* The vast majority of the time the
`interested` list is empty! It is only populated in the short period
of time between when a Peer begins the discovery process and starts
interacting with the discovered peers. That being the case this
operation is probably only useful during development.

### Local View Collection

    /dimple2/localview
      GET - get the list of Peers in the Local View

This allows a caller to get the current list of urls in this peers
Local View. This is not used as part of the shuffle algorithm, but
simple exists for informational and monitoring purposes. This returns
the following

    Array.of(Peer)

This may take 1 parameter `oldest=true`. This will return an array of
a single peer that is the current oldest member of the Local View.

### Local View Item

    /dimple2/localview/{url}
      PUT - Put the url indentified by {url}, initiating a shuffle response

This starts a shuffle process. The body of the `PUT` request should be
a `Peer` object. See the Dimple-II, Algorithm 4,5, and 6 for details.

### Join Local View Collection

    /dimple2/joinlocalview
      GET - get the generated Local View

Provides a generated Local View as described in Dimple-II, Algorithm 7.

### Node Estimate

    /dimple2/nodeestimate
      GET - provides the current estimate of the number of nodes in
            the system. See Dimple-II Algorithm 8.


[^dimpleii]: url to dimple-ii document
[^estimate]: See the section 'Network Size Estimate'
<!--  LocalWords:  api
 -->
