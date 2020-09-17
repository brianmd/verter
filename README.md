# ро́бот ве́ртер

![verter](doc/img/verter.png)

[verter](https://ru.wikipedia.org/wiki/%D0%A0%D0%BE%D0%B1%D0%BE%D1%82_%D0%92%D0%B5%D1%80%D1%82%D0%B5%D1%80) is a curator of institute of time.</br>
his most famous adventure takes place in the year of 2084..

[![<! release](https://img.shields.io/badge/dynamic/json.svg?label=release&url=https%3A%2F%2Fclojars.org%2Ftolitius%2Fverter%2Flatest-version.json&query=version&colorB=blue)](https://github.com/tolitius/verter/releases)
[![<! clojars](https://img.shields.io/clojars/v/tolitius/verter.svg)](https://clojars.org/tolitius/verter)

- [what and why](#what-and-why)
- [how to play](#how-to-play)
    - [create a datasource](#create-a-datasource)
    - [allow verter in](#allow-verter-in)
    - [create institute of time](#create-institute-of-time)
- [adding facts](#adding-facts)
  - [don't record what is already known](#dont-record-what-is-already-known)
- [looking at facts](#looking-at-facts)
  - [facts upto](#facts-upto)
  - [identity now](#identity-now)
  - [identity "as of"](#identity-as-of)
- [add data store](#add-data-store)
    - [implement `Identity` protocol](#implement-identity-protocol)
    - [add a `connect` function](#add-a-connect-function)
    - [create schema](#create-schema)
  - [SQLite](#sqlite)
- [useless benchmarks](#useless-benchmarks)
  - [writes](#writes)
  - [reads](#reads)
- [license](#license)

# what and why

verter is _not_ a database, it is _not_ a data store<br/>
it _relies_ on one though to manage records about identities in space and time.

in contrast to [crux](https://github.com/juxt/crux), [datomic](https://github.com/Datomic) and other temporal databases
the idea behind verter is not to be your database, but to live "inside" an _existing_ database to provide audit, history and other bitemporal human needs.

verter is influenced by work that is done by the [crux](https://github.com/juxt/crux) team. if a bitemporal solution is what you are after, where all the data can live in crux, you should just start using crux instead. verter is for use cases where you already have a database / data store.

# how to play

since verter sits inside the existing database all it needs is a datasource, which could be an existing datasource, or a new one to, for example, have a separate connection pool. but do start from the datasource you already have.

### create a datasource

this example is using a datasource to [Postgres](https://www.postgresql.org/):

```clojure
=> (require '[hikari-cp.core :as hk])
=> (def db (hk/make-datasource {:adapter "postgresql" :url "..."})))
```

until now there is nothing verter specific, we just creating a datasource. check out a working example in [dev.clj](dev/dev.clj).

### allow verter in

```clojure
=> (require '[verter.core :as v]
            '[verter.store :as vs])

=> (def verter (vs/connect :postgres db))
```

regardless of which data store verter is connected to API should behave the same way<br/>
you can [connect with SQLite](#sqlite), if it is easier, then come back and follow the API examples

> _SQLite support is almost done, but still in progress, so there might be some differences_

### create institute of time

in case this is the first time verter is used with this database, the institute of time (verter tables/buckets) need to be created:

```clojure
=> (vs/create-institute-of-time :postgres db)
```

# adding facts

```clojure
=> (v/add-facts verter [{:verter/id :universe/one :suns 12 :planets #{:one :two :three}}
                        [{:verter/id :universe/two :suns 3 :life? true} #inst "2019-09-09"]
                        {:verter/id :universe/sixty-six :answer 42}])

{:tx-id #uuid "5f62d4c0-c5ca-4431-ac33-610825f9ef36"}
```

there are a few things to mention here:

* every identity needs to have a `:verter/id`
* facts about `:universe/two` were given a "business time"
* other facts will assume that their business time is the transaction time
* all these facts were recorded in a single transaction
* this transaction id ("`tx-id`") is `5f62d4c0-c5ca-4431-ac33-610825f9ef36`

## don't record what is already known

let's try this again:

```clojure
=> (v/add-facts verter [[{:verter/id :universe/two :suns 3 :life? true} #inst "2019-09-09"]])

{:noop "no changes to identities were detected, and hence, these facts
        [[{:verter/id :universe/two, :suns 3, :life? true} #inst \"2019-09-09T00:00:00.000-00:00\"]]
        were NOT added at 2020-09-17T03:16:58.774118Z"}
```

since we already know this fact about `:universe/two` at this business time.

but.., let's update `:universe/two` business time:

```clojure
=> (v/add-facts verter [[{:verter/id :universe/two :suns 3 :life? true} #inst "2020-09-09"]])

{:tx-id #uuid "5f62d5af-c4e1-475d-b086-3dad13015623"}
```

same "recorded" result would be without a business time, since transaction time will be used instead (which changes.. one planck at a time):

```clojure
=> (v/add-facts verter [[{:verter/id :universe/two :suns 3 :life? true}]])
{:tx-id #uuid "5f62d6b3-df46-4d76-9b71-d8bc3619d791"}
```

you might have noticed that transaction ids are sequential UUIDs. good catch!

# looking at facts

```clojure
=> (v/facts verter :universe/sixty-six)

[{:answer 42,
  :verter/id :universe/sixty-six,
  :at #inst "2020-09-17T03:15:12.859748000-00:00"}]
```

`facts` function will return all the changes to an identity over time:

let's add more suns to `:universe/one`:

```clojure
=> (v/add-facts verter [[{:verter/id :universe/one :suns 42}]])

{:tx-id #uuid "5f62d75d-af6e-4a1d-8687-fc1ee690d1f4"}
```

and look at how universe was doing its magic by adding more suns over time:

```clojure
=> (v/facts verter :universe/one)

[{:suns 12,
  :planets #{:one :three :two},
  :verter/id :universe/one,
  :at #inst "2020-09-17T03:15:12.859748000-00:00"}
 {:suns 42,
  :verter/id :universe/one,
  :at #inst "2020-09-17T03:26:21.253047000-00:00"}]
```

"facts" would also show "business time" changes:

```clojure
=> (v/facts verter :universe/two)

[{:suns 3,
  :life? true,
  :verter/id :universe/two,
  :at #inst "2019-09-09T00:00:00.000000000-00:00"}
 {:suns 3,
  :life? true,
  :verter/id :universe/two,
  :at #inst "2020-09-09T00:00:00.000000000-00:00"}
 {:suns 3,
  :life? true,
  :verter/id :universe/two,
  :at #inst "2020-09-17T03:23:31.132664000-00:00"}]
```

i.e. :point_up_2: no facts were really changed, but there was a change in "business time" that we did earlier.

ah, yes "`:at`" is "business time". good catch!

### with transaction intel

you may add `{:with-tx? true}` to see the transaction intel across history:

```clojure
=> (v/facts verter :universe/two {:with-tx? true})

[{:suns 3,
  :life? true,
  :verter/id :universe/two,
  :at #inst "2019-09-09T00:00:00.000000000-00:00",
  :tx-time #inst "2020-09-17T03:15:12.859748000-00:00",
  :tx-id #uuid "5f62d4c0-c5ca-4431-ac33-610825f9ef36"}
 {:suns 3,
  :life? true,
  :verter/id :universe/two,
  :at #inst "2020-09-09T00:00:00.000000000-00:00",
  :tx-time #inst "2020-09-17T03:19:11.341449000-00:00",
  :tx-id #uuid "5f62d5af-c4e1-475d-b086-3dad13015623"}
 {:suns 3,
  :life? true,
  :verter/id :universe/two,
  :at #inst "2020-09-17T03:23:31.132664000-00:00",
  :tx-time #inst "2020-09-17T03:23:31.132664000-00:00",
  :tx-id #uuid "5f62d6b3-df46-4d76-9b71-d8bc3619d791"}]
```

## facts upto

let's add some more facts about the `:universe/sixty-six`:

```clojure
=> (v/add-facts verter [{:verter/id :universe/sixty-six :suns 42 :planets #{:and-all :earth}, :life? true}
                        {:verter/id :universe/sixty-six :moons 42}
                        {:verter/id :universe/sixty-six :moons nil}])

{:tx-id #uuid "5f62d86d-b7ac-4821-ad65-70ced2f5e95a"}
```

so now it looks like this:

```clojure
=> (v/facts verter :universe/sixty-six)

[{:answer 42,
  :verter/id :universe/sixty-six,
  :at #inst "2020-09-17T03:15:12.859748000-00:00"}
 {:suns 42,
  :planets #{:and-all :earth},
  :life? true,
  :verter/id :universe/sixty-six,
  :at #inst "2020-09-17T03:30:53.388072000-00:00"}
 {:moons 42,
  :verter/id :universe/sixty-six,
  :at #inst "2020-09-17T03:30:53.388072000-00:00"}
 {:moons nil,
  :verter/id :universe/sixty-six,
  :at #inst "2020-09-17T03:30:53.388072000-00:00"}]
```

when looking for facts we can specify a certain time upto which the facts are needed:

```clojure
=> (v/facts verter
            :universe/sixty-six
            {:upto #inst "2020-09-17T03:20:00"})

[{:answer 42,
  :verter/id :universe/sixty-six,
  :at #inst "2020-09-17T03:15:12.859748000-00:00"}]
```

since the other 3 facts were added a bit after "`2020-09-17T03:20:00`", there is only one fact that "matter" in this case.

## identity now

continuing with `:universe/sixty-six` with currently 4 facts:

```clojure
[{:answer 42,
  :verter/id :universe/sixty-six,
  :at #inst "2020-09-17T03:15:12.859748000-00:00"}
 {:suns 42,
  :planets #{:and-all :earth},
  :life? true,
  :verter/id :universe/sixty-six,
  :at #inst "2020-09-17T03:30:53.388072000-00:00"}
 {:moons 42,
  :verter/id :universe/sixty-six,
  :at #inst "2020-09-17T03:30:53.388072000-00:00"}
 {:moons nil,
  :verter/id :universe/sixty-six,
  :at #inst "2020-09-17T03:30:53.388072000-00:00"}]
```

a quite frequent need is to see the identity "now": i.e. all the facts rolled up over time.

this can be done with `v/rollup`:

```clojure
=> (v/rollup verter :universe/sixty-six)

{:answer 42,
 :verter/id :universe/sixty-six,
 :at #inst "2020-09-17T03:30:53.388072000-00:00",
 :suns 42,
 :planets #{:and-all :earth},
 :life? true}
```

one interesting "fact" about this rollup example: the `moons`.
the last fact about the `moons` is that it is "nil", hence while it still shows up in the list of facts, unless it has a value, it won't show up in a rollup.

## identity "as of"

similarly to the "identity now" rollup above, we can look at an identity at a given point in time a.k.a. "as of" time:

let's add a fact about the moons right before that transaction with 3 facts above:

```clojure
=> (v/add-facts verter [[{:verter/id :universe/sixty-six :moons 13} #inst "2020-09-17T03:20:00"]])

{:tx-id #uuid "5f62de48-14c9-4acb-99d6-82e721d50443"}
```

now `:universe/sixty-six` looks like this:

```clojure
=> (v/facts verter :universe/sixty-six)

[{:answer 42,
  :verter/id :universe/sixty-six,
  :at #inst "2020-09-17T03:15:12.859748000-00:00"}
 {:moons 13,                                         ;; here is out "new" moon count
  :verter/id :universe/sixty-six,
  :at #inst "2020-09-17T03:20:00.000000000-00:00"}   ;; with its own busines time
 {:suns 42,
  :planets #{:and-all :earth},
  :life? true,
  :verter/id :universe/sixty-six,
  :at #inst "2020-09-17T03:30:53.388072000-00:00"}
 {:moons 42,
  :verter/id :universe/sixty-six,
  :at #inst "2020-09-17T03:30:53.388072000-00:00"}
 {:moons nil,
  :verter/id :universe/sixty-six,
  :at #inst "2020-09-17T03:30:53.388072000-00:00"}]
```

see that second fact about the number of moons, but with the earlier business time `03:20` vs. `03:30`?

let's look at this identity (`:universe/sixty-six`) upto this business time:

```clojure
=> (v/as-of verter :universe/sixty-six #inst "2020-09-17T03:20:00")

{:answer 42,
 :verter/id :universe/sixty-six,
 :at #inst "2020-09-17T03:20:00.000000000-00:00",
 :moons 13}
```

as of "`2020-09-17T03:20:00`" this universe had 13 moons and the answer was and still is.. 42.

# add data store

supported data stores:
| data store | supported  | in progress  |
|---|:-:|:-:|
| PostgreSQL  | :white_check_mark: |                      |
| SQLite      |                    |  :white_check_mark:  |
| MySQL       |  |  |
| Redis       |  |  |
| Cassandra   |  |  |
| Couchbase   |  |  |
| MsSQL       |  |  |
| Oracle      |  |  |
| Your store  |  |  |

:heart: consider contributing to add a support for a data store that is used by your applications.

adding a missing data store comes down to 1, 2, 3

### implement `Identity` protocol

```clojure
(defprotocol Identity
  (facts [this id]
         [this id opts]
    "find all the facts about the identity until now or with options")

  (add-facts [this facts]
    "add one or more facts with or without time specified
     if time is not specified, a single transaction time is used")

  (obliterate [this id]
     "'big brother' move: the idenitity never existed"))
```

take a look at [postgres](https://github.com/tolitius/verter/blob/ffba06f9c2001ff31c2e8aaecf4ac401ee26e262/src/verter/store/postgres.clj#L63-L77) implementation to get an idea.

### add a `connect` function

take a look at [postgres](https://github.com/tolitius/verter/blob/ffba06f9c2001ff31c2e8aaecf4ac401ee26e262/src/verter/store/postgres.clj#L81-L87) connect function.

the idea is to take a datasource of an existing data store and create an instance of the `Identity` protocol above.

### create schema

depending on what a data store is you might (or not) need to create a schema where verter will keep facts about identities and transactions. i.e. make a function that will create a schema.

of course redis "schema" would probably be just a couple of bucket names vs. a SQL database schema would need to create a couple of tables.

take a look at the [postgres schema](https://github.com/tolitius/verter/blob/ffba06f9c2001ff31c2e8aaecf4ac401ee26e262/resources/verter/store/postgres/create-institute-of-time.sql) to get an idea.

----

if you are interested and/or need more details, just open an [issue](https://github.com/tolitius/verter/issues), and let's talk

## SQLite

here is an example of using verter with a [SQLite database](https://sqlite.org/index.html):

```clojure
;; this is from verter/dev.clj, but you could of course create a datasource on your own
;; i.e. (hk/make-datasource {:jdbc-url "jdbc:sqlite:db/database.db"})

=> (def db (start-db :sqlite))
```

```clojure
=> (def verter (vs/connect :sqlite db))
=> (vs/create-institute-of-time :sqlite db)

=> (v/add-facts verter [{:verter/id :universe/one :suns 12 :planets #{:one :two :three}}
                        [{:verter/id :universe/two :suns 3 :life? true} #inst "2019-09-09"]
                        {:verter/id :universe/sixty-six :answer 42}])

=> (v/add-facts verter [{:verter/id :universe/one :suns 42 :planets #{:one :two :three}}
                        [{:verter/id :universe/two :suns 3 :life? true} #inst "2020-09-09"]
                        {:verter/id :universe/sixty-six :answer 42}])
```

```clojure
=> (pprint (v/facts verter :universe/one))
[{:suns 12,
  :planets #{:one :three :two},
  :verter/id :universe/one,
  :at #inst "2020-09-15T18:33:23.736-00:00"}
 {:suns 42,
  :planets #{:one :three :two},
  :verter/id :universe/one,
  :at #inst "2020-09-15T18:33:42.035-00:00"}]
```

and here is what's brewing inside the database:

```
$ sqlite3 dev/verter.db

sqlite> select * from facts;
1|:universe/one|NPY|8cc82db19a67cc61e8e4144a854c15fd|1600194803736
2|:universe/two|NPY|0804a4e066f308756dcab8aa1dd35ae4|1567987200000
3|:universe/sixty-six|NPY|b9bb678c38b5a29917a4a7baafdbf754|1600194803736
4|:universe/one|NPY|8657a740c448d4fc46af054f94ed5cec|1600194822035
5|:universe/two|NPY|6ef240c23aee0ecef51d710c09675022|1599609600000

sqlite> select * from transactions;
1|1600194803736|NPY.ri 20bd8..
2|1600194822035|NPY.ri c29c5..
```

# useless benchmarks

since most of the work is done directly against the database, let's pretend the asking party is collocated with such database:

these are the numbers on top of Postgres:

## writes

we'll create a few new universes to avoid "do nothing" tricks verter does on hash matches:

```clojure
=> (def u40 (mapcat #(vector {:verter/id :universe/forty     :moons % :suns %}) (range 5500)))
=> (def u41 (mapcat #(vector {:verter/id :universe/forty-one :moons % :suns %}) (range 5500)))
=> (def u42 (mapcat #(vector {:verter/id :universe/forty-two :moons % :suns %}) (range 5500)))
```

and add them sequentially in a single thread, not even with "pmap":

```clojure
=> (time (do (v/add-facts verter u40)
             (v/add-facts verter u41)
             (v/add-facts verter u42)))

"Elapsed time: 1009.084383 msecs"
```

`3 * 5500` = `16,500` a second.<br/>

later we'll play with type hints, threads, smaller batch sizes would also help..</br>
but the most time will always be in I/O, so this is close.

## reads

let's read facts for `:universe/two` that has four entries and looks like this:

```clojure
=> (v/facts verter :universe/two)
[{:suns 42,
  :moons nil,
  :life? true,
  :verter/id :universe/two,
  :at #inst "2020-09-10T22:33:20.204008000-00:00"}
 {:suns 42,
  :moons 12,
  :life? true,
  :verter/id :universe/two,
  :at #inst "2020-09-10T22:10:36.352157000-00:00"}
 {:suns 42,
  :life? true,
  :verter/id :universe/two,
  :at #inst "2020-09-10T22:10:22.712308000-00:00"}
 {:suns 3,
  :life? true,
  :verter/id :universe/two,
  :at #inst "2019-09-09T00:00:00.000000000-00:00"}]
```

```clojure
=> (time (dotimes [_ 6500] (v/facts verter :universe/two)))
"Elapsed time: 1072.743808 msecs"
```

these benchmarks are "entertainment" hence no "criterium", multiple threads, connection pool tricks, light speed network latency, etc..

# license

Copyright © 2020 tolitius

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
