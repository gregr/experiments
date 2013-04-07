-- Playing with equational reasoning of untyped recursion combinators, seeing
-- where they lead and how to simplify the results.  Simplification seems to
-- gravitate towards a graph representation (involving letrec).


non-strict:

uu = \f x -> f (x x)

yc = \e -> (\u -> u u) (uu e)


(\u -> u u) (uu e)

(\u -> u u) (\x -> e (x x))

(\x -> e (x x)) (\x -> e (x x))

e ((\x -> e (x x)) (\x -> e (x x)))

================================================================
strict:

id = \z -> z

uu = \f x -> f (delay (x x))

yc = \e -> (uu id) (uu e)


(uu id) (uu e)

(\x -> (\z -> z) (delay (x x))) (uu e)

(\x -> (delay (x x))) (uu e)

(\x -> (delay (x x))) (\x -> e (delay (x x)))

(delay ((\x -> e (delay (x x))) (\x -> e (delay (x x)))))

-- force

((\x -> e (delay (x x))) (\x -> e (delay (x x))))

e (delay ((\x -> e (delay (x x))) (\x -> e (delay (x x)))))

================================================================
strict mutual-recursion:

letrec
  a = \a-args -> a-def
  b = \b-args -> b-def
  ...

\self ->
  {delay ((\{a, b, ...} -> (\a-args -> a-def)) (self self)),
   delay ((\{a, b, ...} -> (\b-args -> b-def)) (self self)),
   ...}

\fself ->
  (\self ->
    {delay ((\{a, b, ...} -> (\a-args -> a-def)) (force self)),
     delay ((\{a, b, ...} -> (\b-args -> b-def)) (force self)),
     ...}) (delay (fself fself))

mu h0.
  (\fself ->
    (\self ->
      {delay ((\{a, b, ...} -> (\a-args -> a-def)) (force self)),
       delay ((\{a, b, ...} -> (\b-args -> b-def)) (force self)),
       ...}) (delay (fself fself)))
  (\fself ->
    (\self ->
      {delay ((\{a, b, ...} -> (\a-args -> a-def)) (force self)),
       delay ((\{a, b, ...} -> (\b-args -> b-def)) (force self)),
       ...}) (delay (fself fself)))

mu h0.
  (\self ->
    {delay ((\{a, b, ...} -> (\a-args -> a-def)) (force self)),
     delay ((\{a, b, ...} -> (\b-args -> b-def)) (force self)),
     ...}) (delay (
              (\fself ->
                (\self ->
                  {delay ((\{a, b, ...} -> (\a-args -> a-def)) (force self)),
                   delay ((\{a, b, ...} -> (\b-args -> b-def)) (force self)),
                   ...}) (delay (fself fself)))
              (\fself ->
                (\self ->
                  {delay ((\{a, b, ...} -> (\a-args -> a-def)) (force self)),
                   delay ((\{a, b, ...} -> (\b-args -> b-def)) (force self)),
                   ...}) (delay (fself fself)))
            ))

mu h0.
  (\self ->
    {delay ((\{a, b, ...} -> (\a-args -> a-def)) (force self)),
     delay ((\{a, b, ...} -> (\b-args -> b-def)) (force self)),
     ...}) (delay (h0))

mu h0.
  {delay ((\{a, b, ...} -> (\a-args -> a-def)) h0),
   delay ((\{a, b, ...} -> (\b-args -> b-def)) h0),
   ...}

mu h0.
  {delay (\a-args -> a-def[h0@0/a, h0@1/b, ...]),
   delay (\b-args -> b-def[h0@0/a, h0@1/b, ...]),
   ...}

mu h0.
  {(\a-args -> a-def[h0@0/a, h0@1/b, ...]),
   (\b-args -> b-def[h0@0/a, h0@1/b, ...]),
   ...}

-- what notation for a graph with mutual cycles? seems like we end up back at letrec
-- letrec (for SCCs) and let (for acyclic dependencies) should be primitive after all
--   this is a much more direct way of representing dynamically-generated term graphs

{-\self ->-}
  {-{\a-args -> (\{a, b, ...} -> a-def) (self self),-}
   {-\b-args -> (\{a, b, ...} -> b-def) (self self),-}
   {-...}-}

{-yc (\self -> (\{a, b, ...} -> { adef, bdef, ... }) (force self))-}

{-e (delay ((\x -> e (delay (x x))) (\x -> e (delay (x x)))))-}
