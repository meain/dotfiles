# Code Smells & Refactoring Catalog

Authoritative vocabulary for the `refactorability` skill. Based on Martin Fowler's
*Refactoring* (2nd ed.) and refactoring.guru. When citing a smell or a refactoring move
in a review, **use a name from this file**. If a finding doesn't fit, call it a "Note",
not a finding.

The format for each entry is:

> **Smell** — short definition. *Signals:* what to look for. *Move(s):* refactoring(s) to apply.

---

## Bloaters

Things that grew too big to manage.

- **Long Method** — a function that does too much.
  *Signals:* > ~50 LOC; multiple "and"s in its name; comments dividing it into sections.
  *Moves:* Extract Method · Replace Temp with Query · Introduce Parameter Object · Decompose Conditional.

- **Large Class** — a class/struct with too many fields or responsibilities.
  *Signals:* many unrelated fields; methods clustered into clear groups; co-changing subsets.
  *Moves:* Extract Class · Extract Subclass · Extract Interface.

- **Long Parameter List** — > ~4 parameters, or any list with multiple flags/options.
  *Signals:* boolean flags; repeated subsets across call sites.
  *Moves:* Introduce Parameter Object · Preserve Whole Object · Replace Parameter with Query · Remove Flag Argument.

- **Primitive Obsession** — using primitives where a small type would carry meaning.
  *Signals:* `string` UserID/Email/URL passed everywhere; magic constants; validation duplicated at call sites.
  *Moves:* Replace Primitive with Object · Replace Type Code with Class/Subclasses · Extract Class.

- **Data Clumps** — the same 3+ fields/parameters appear together repeatedly.
  *Signals:* `(x, y, z)` or `(start, end)` repeated across signatures and structs.
  *Moves:* Extract Class · Introduce Parameter Object · Preserve Whole Object.

---

## Object-Orientation Abusers

Incomplete or wrong application of OO/abstraction.

- **Switch Statements** — `switch`/`if` ladders on a type tag, repeated in several places.
  *Signals:* same `switch kind {}` in N functions; adding a case requires editing N sites.
  *Moves:* Replace Conditional with Polymorphism · Replace Type Code with Subclasses · Replace Conditional with Strategy.

- **Refused Bequest** — subclass inherits things it doesn't want, overrides to no-op or panic.
  *Signals:* `func (X) Foo() { panic("not supported") }`.
  *Moves:* Replace Subclass with Delegate · Replace Superclass with Delegate · Push Down Method/Field.

- **Alternative Classes with Different Interfaces** — two types do similar things but expose different APIs.
  *Signals:* `LoadX`/`FetchY` doing the same shape of work.
  *Moves:* Rename Method · Move Method · Extract Superclass/Interface.

- **Temporary Field** — a field that's only set/used in some cases.
  *Signals:* nil-checks on a struct field that's only populated by one method.
  *Moves:* Extract Class · Introduce Special Case · Preserve Whole Object.

---

## Change Preventers

When you *can* change the code but a small change costs disproportionately.

- **Divergent Change** — one module changes for many unrelated reasons.
  *Signals:* the same file appears in PRs for unrelated features.
  *Moves:* Split Phase · Extract Class · Move Function.

- **Shotgun Surgery** — one logical change forces edits in many files.
  *Signals:* "to add a field, touch handler + DTO + mapper + repo + test fixtures".
  *Moves:* Move Function · Move Field · Combine Functions into Class · Inline Class.

- **Parallel Inheritance Hierarchies** — adding a subclass in tree A forces one in tree B.
  *Signals:* `FooHandler` ↔ `FooService` ↔ `FooDTO` tied 1:1.
  *Moves:* Move Function · Move Field (collapse one hierarchy into the other).

---

## Dispensables

Things you can delete and the code gets better.

- **Duplicated Code** — same logic in 2+ places.
  *Signals:* `dupl` output; copy-pasted blocks; same `if err != nil { log; return }` rituals.
  *Moves:* Extract Function · Pull Up Method · Form Template Method · Slide Statements.
  *Caveat:* rule of three — two duplicates aren't necessarily a smell yet.

- **Dead Code** — unreachable, unused, or always-false branches.
  *Signals:* `vulture` output; functions with no callers; flags never flipped.
  *Moves:* Delete it.

- **Lazy Class** — a class/struct that doesn't earn its keep.
  *Signals:* a struct with one field and one passthrough method.
  *Moves:* Inline Class · Collapse Hierarchy.

- **Data Class** — only fields and getters/setters, no behavior.
  *Signals:* DTO-like type whose callers all do the same computation on it.
  *Moves:* Move Function (push behavior in) · Encapsulate Record.
  *Caveat:* legitimate DTOs at API boundaries are fine.

- **Speculative Generality** — abstractions with one caller waiting for "future flexibility".
  *Signals:* interface with one implementation; unused hooks; configurable knobs no one sets.
  *Moves:* Inline Function · Inline Class · Collapse Hierarchy · Remove Dead Code.

- **Comments (as deodorant)** — comments explaining *what* convoluted code does.
  *Signals:* a comment introducing a block that could be a named function.
  *Moves:* Extract Function (use the comment as the function name) · Rename Variable.
  *Caveat:* comments explaining *why* are valuable; keep them.

---

## Couplers

Excessive entanglement between modules.

- **Feature Envy** — a method uses another object's data more than its own.
  *Signals:* `other.X + other.Y + other.Z` repeated; getter chains.
  *Moves:* Move Function · Extract Function then move it.

- **Inappropriate Intimacy** — two modules know too much about each other's internals.
  *Signals:* reaching into private/unexported fields; mutual back-pointers.
  *Moves:* Move Function/Field · Change Bidirectional to Unidirectional · Extract Class · Hide Delegate.

- **Message Chains** — `a.b().c().d().e()`.
  *Signals:* Demeter violations; tests breaking when intermediate types change.
  *Moves:* Hide Delegate · Extract Function.

- **Middle Man** — a class that just delegates everything to another.
  *Signals:* > half its methods are 1-line passthroughs.
  *Moves:* Remove Middle Man · Inline Function.

---

## SOLID lens

Higher-level patterns that often surface as combinations of the above.

- **SRP (Single Responsibility)** — module changes for more than one reason.
  *Signals:* matches Divergent Change; Large Class.
  *Moves:* Extract Class · Split Phase.

- **OCP (Open/Closed)** — adding a case requires editing existing code rather than extending it.
  *Signals:* matches Switch Statements on a type tag.
  *Moves:* Replace Conditional with Polymorphism · Replace Conditional with Strategy.

- **LSP (Liskov)** — subtype isn't substitutable; overrides throw or weaken contracts.
  *Signals:* matches Refused Bequest.
  *Moves:* Replace Subclass with Delegate · Push Down Method.

- **ISP (Interface Segregation)** — consumers depend on methods they don't use.
  *Signals:* fat interface; mocks with most methods stubbed out.
  *Moves:* Extract Interface · Split Interface.

- **DIP (Dependency Inversion)** — high-level code depends on concrete low-level types.
  *Signals:* direct `*sql.DB` in business logic; `time.Now()` sprinkled in domain code.
  *Moves:* Extract Interface · Inject Dependency · Introduce Seam.
  *Caveat:* don't introduce interfaces with one implementation just to "follow DIP" — that's Speculative Generality.

---

## Quantitative thresholds (rules of thumb)

Treat these as hints, not laws. Always validate with the surrounding context.

| Signal                       | Yellow flag | Red flag |
|------------------------------|-------------|----------|
| Cyclomatic complexity        | > 10        | > 20     |
| Cognitive complexity         | > 10        | > 20     |
| Function length              | > 50 LOC    | > 100 LOC |
| Parameters                   | > 4         | > 6      |
| Nesting depth                | > 3         | > 5      |
| Class/struct size            | > 200 LOC   | > 500 LOC |
| Duplicate token block        | > 50 tokens | > 100 tokens |
| Fan-in (importers of pkg)    | -           | flag if also large |
| Fan-out (deps of single fn)  | > 5         | > 10     |

---

## Refactoring moves (quick index)

When suggesting a fix, pick a name from this list.

**Composing functions:** Extract Function · Inline Function · Extract Variable · Inline Variable · Change Function Declaration · Encapsulate Variable · Rename Variable · Introduce Parameter Object · Combine Functions into Class · Combine Functions into Transform · Split Phase.

**Moving features:** Move Function · Move Field · Move Statements into Function · Move Statements to Callers · Slide Statements · Split Loop · Replace Loop with Pipeline · Remove Dead Code.

**Organizing data:** Split Variable · Replace Derived Variable with Query · Change Reference to Value · Replace Primitive with Object · Replace Magic Literal.

**Simplifying conditionals:** Decompose Conditional · Consolidate Conditional Expression · Replace Nested Conditional with Guard Clauses · Replace Conditional with Polymorphism · Replace Conditional with Strategy · Introduce Special Case · Introduce Assertion · Remove Flag Argument.

**API refactorings:** Separate Query from Modifier · Parameterize Function · Replace Parameter with Query · Replace Query with Parameter · Preserve Whole Object · Remove Setting Method · Replace Constructor with Factory Function · Return Modified Value.

**Dealing with inheritance:** Pull Up Method/Field · Push Down Method/Field · Extract Superclass · Extract Subclass · Extract Interface · Replace Subclass with Delegate · Replace Superclass with Delegate · Collapse Hierarchy.

**Encapsulation:** Encapsulate Record · Encapsulate Collection · Hide Delegate · Remove Middle Man · Substitute Algorithm · Change Bidirectional to Unidirectional.
