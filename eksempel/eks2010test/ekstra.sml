fun tassign1 "The pope sleeps" = true
  | tassign1 "The pope snores" = false
  | tassign1 "The pope practices gymnastics" = false
  | tassign1 "The pope preaches in his sleep" = true
  | tassign1 "ERROR" = raise General.Fail "Shouldn't happen"
  | tassign1 _ = false


val kaltjek =
    let
      val kal = [((11, 11, 1968), "Møde med JS"),
                 ((23, 10, 2010), "middag hos Aase"),
                 ((9, 11, 2010), "Dansk Datahistorisk Forening"),
                 ((11, 8, 2011), "Jane & Svends sølvbryllup")]
      val ord = String.tokens Char.isSpace
      fun eq1 (((a, m, d), s), ((a', m', d'), s')) =
          m = m' andalso
          (a = a' andalso d = d' orelse a = d' andalso d = a') andalso
          ord s = ord s'
      fun eq (xs, ys) = length xs = length ys andalso
                        List.all eq1 $ ListPair.zip (xs, ys)
    in
      aekvivalens
        "_"
        eq
        kal
    end

fun klassetjek x =
    aekvivalens
      "_"
      (fn (xsss, ysss) =>
          let
            fun klasseeq xss yss =
                let
                  fun perm xs ys = Set.equal (Set.fromList xs) (Set.fromList ys)
                  fun leq (xss, yss) =
                      List.all (fn xs => List.exists (perm xs) yss) xss
                in
                  leq (xss, yss) andalso leq (yss, xss)
                end
            fun leq (xsss, ysss) =
                List.all (fn xss => List.exists (klasseeq xss) ysss) xsss
          in
            leq (xsss, ysss) andalso leq (ysss, xsss)
          end
      )
      x

fun klisttjek ind =
    (fn ud => TrivKList.tilListe (Lazy.force ud) = ind,
     [Test.Beskrivelse "k-listen svarende til _"]
    )

fun equiv (a, b) =
    let
      fun fv (NOT p) = fv p
        | fv (AND (p1, p2)) = fv p1 @ fv p2
        | fv (OR (p1, p2)) = fv p1 @ fv p2
        | fv (VAR s) = [s]
        | fv _ = nil
      fun powerset nil = [nil]
        | powerset (x :: xs) =
          let
            val pxs = powerset xs
          in
            pxs @ map (x \< op::) pxs
          end
      fun assign nil _ = false
        | assign (x :: xs) s = x = s orelse assign xs s
      val fva = fv a
      val fvb = fv b
      val fv = Set.toList $ Set.union (Set.fromList fva) (Set.fromList fvb)
    in
      List.all (fn t => Vejl.eval a t = Vejl.eval b t)
               (map assign $ powerset fv)
    end

fun simp TT = true
  | simp FF = true
  | simp p =
    let
      fun nobools TT = false
        | nobools FF = false
        | nobools (NOT p) = nobools p
        | nobools (AND (p1, p2)) = nobools p1 andalso nobools p2
        | nobools (OR (p1, p2)) = nobools p1 andalso nobools p2
        | nobools (VAR s) = true
    in
      nobools p
    end

fun impliestjek x =
    aekvivalens
      "et udsagn aekvivalent med _"
      equiv
      x

fun simptjek ventet ind =
    (fn ud => equiv (ind, Lazy.force ud) andalso simp $ Lazy.force ud,
     [Test.Vaerdi (ventet, "simpelt udtryk aekvivalent med _")]
    )
