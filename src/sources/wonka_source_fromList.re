open Wonka_types;
open Wonka_helpers;

[@genType]
let fromList = (ls: list('a)): sourceT('a) =>
  curry(sink => {
    let value = ref(ls);

    makeTrampoline(sink, (.) =>
      switch (value^) {
      | [x, ...rest] =>
        value := rest;
        Some(x);
      | [] => None
      }
    );
  });
