Lisp Is a Sanskrit Parallel
==========
Verson 1: A Quick 'n' Dirty Elaboration for CS People
----------

In the following note, I first sketch out direct parallels and then demonstrate them with examples.
 * I rely on the precise and closely reasoned terminology of the SICP text book in order to spell out identical design and implementation features of both Lisp and Sanskrit.
 * CS people will recognise all these ideas and so, to effect "A Quick 'n' Dirty Elaboration", I only use two examples from Vedic Mathematics to demonstrate nearly all the direct parallels.
 * Many more subtle nuances are left for an expanded "Version 2"; to be completed.
 * None of the ideas contained below originally occured to me. I am only putting two-and-two together and sketching a picture.

# Striking Parallels

Lisp is, first and foremost, a language standard. And so is Sanskrit.
 * Both standards rely upon the pre-existance of atomic constructs that are universal and near-unchanging. Lambda Calculus is to Lisp, what Phoenetics is to Sanskrit.
 * Both standards were evolved to progressively control and manage complexities arising from the desire to implement general-purpose knowledge processing at any scale.
 * Both standards were codified to ensure undistorted and timeless propagation of _underlying knowledge processing principles_ without being burdened by dependance on any single - and by nature, arbitrary - language implementation.
 * This holds true even though the Lisp standard evolved to fit the medium of essentially serial-processing hardware, whereas the Sanskrit standard evolved to fit the medium of essentially networked-processing wetware.

Both standards provide, in full measure, all three key mechanisms common to all powerful programming languages:
 * Primitive expressions, which represent the simplest entities the language is concerned with,
 * Means of Combinination, by which Compound Elements are built, and
 * Means of Abstraction, by which compound elements can be named and manipulated as units. Very strikingly, both foster Procedural Abstraction, Higher Order Procedures including General Methods of computation, Compound Data and Data Abstraction, all the way up to Metalingual Abstraction.

Both standards have historically spawned highly similar dialectic evolution.
 * Each standard first spawned a succinct, expressive, fully general purpose, classical implementation.
 * Standardization enabled several brand new languages as well as derivative dialects, including general-purpose and/or special-purpose variants.
 * Common Lisp is to the Lisp standard, what Vedic Sanskrit is to the Sanskrit Standard. 
 * Scheme, Clojure, Auto-Lisp etc. are to Lisp/Common Lisp what Pali, Prakrit, Marathi etc. are to Sanskrit/Vedic Sanskrit.

Both standards call for implementation of _interpreted_ languages:
 * The fact of "interpretedness" stays true even though Computers and People, so far, possess fundamentally different operating environments. In other words we use different computational processes. On one hand, the processes and time spans of interpreter installation are different. Computers can be "trained" to interpret Lisp implementations with one-click-installs. And people must be trained to use Sanskrit interpretations via repeated recitation of pre-defined material using pre-defined methods. But on the other hand, whereas one computer may operate with the Global Environment of only one Lisp interpreter at run time; one person may concurrently operate with several Sanskrit-based as well as non-Sanskrit-based interpreters at run time.
 * Philosophically, users are allowed to modify the language and even the interpreter itself. In both cases, success is normally follows (say) one or two decades of broad and deep exposure to _solving real problems_ using at least one standard language implementation. (Interim newbie experiments may of course be conducted, but only in the seclusion of a favourite (and preferably far away) forest clearing.)

# Two Illustrative Examples

Pi-expansion Shloka
 * Demonstrates Primitive Expressions, Means of Combination, and 
 * Is a model of Succinctness and Expressiveness
 * Directly demonstrates Multilingual Abstraction
 * Can be further re-interpreted and transformed into a far more profound set of abstractions

Vedic Math Sutra
 * Compare orders of growth
 * Demonstrates one Extra-Important additional idea of, at least, the Vedic Math meta-Sanskrit: The Formula _is_ The Procedure (In Sanskrit, severally, imperative and declarative knowledge is identical.)
 * Possible to demonstrate a macro if the Sutra has a general enough scope

# Conclusion and Next Steps

A "Version 2" subtitled "A More Generalist Elaboration" is to follow. But first, a quick note: 
I _deeply_ believe, "Version 3: A Truly All-Encompassing Elaboration" needs to be written. I am not, as yet, capable of it but there are plenty who are. And I hope that their work will be entitled "Sanskrit Interpreted Computation and Programming", in deference to the high standard of the modern work that so completely and delightfully imparts the reasoning necessary to express not only the poetic verisimilitude but also the hard-nosed engineering design features that are identically leveraged by the world of Lisp as well as the world of Sanskrit.

----------------------------------------------------------------------------------------------------------

# POSTSCRIPT: Fantasy-filled Food For Thought

All CS people (particularly Lispers) know this well:
A particular procedure may require itself to be augmented, at run time, by another procedure or even a complete program, outside of itself, as a _precondition_ for complete and/or error-free execution.

Now consider this: 
It is a widespread and well known Sanskritic tradition to invoke _the name_ of an appropriate "God" _before commencing_ a certain type of task. Such an invocation is done in a very particular manner viz "Aum <God X's name> Namaha". 

This may readily translate to: 
 * Interrupt to Modify Global Environment (procedure named "Aum") + 
 * Load all known procedures associated with procedure-as-parameter <God X's name> + 
 * Parameter to modify behaviour of procedure God X's name. In this case <Namaha> modifies it to "I Bow to Thee and Commence."

This is of course sheer fantasy. But just _suppose_ one entertains the idea. Could it at least be in the _realm_ of possibility?

Here is a fantasy-filled example:
By convention, one chants "Om Ganapatayay Namaha" _prior_ to commencing any study of any field of knowledge.

The plot thickens by way of this deconstruction:

**Symbolically**, the word "Ganapati" is associated with an _abstract being_ capable of flawless and tireless capture of _all_ knowledge. This expands into transcribing all audio-visual input into phoenetically perfect inscribed output while at the same time committing the expanded contextual understanding to memory.

**Procedurally**, let me direct attention to the 6th Canto of the popular prayer-book, the Ganapati Athava-sheersham. It is said to be the beeja-mantram (atomic representation) of the root sound "Gam" that creates the word Ganapati. Close analysis reveals that the linguistic and typographic key to Devnagri is coded therein:

 > Decode and Explain: Ganapati Stotra's Beeja Mantram.

**Process-wise**, the standard Vedic system of training looks like a model for the _process_ that could be generated by the Procedures encapsulated under Ganapati's name:
 * Perform phoenetically, metrically and rhythimically accurate chanting of text matter inscribed as verse
 * Do this in combination with very specific hand motions to reinforce meter, rhythm, and inflections. By convention, only the dominant hand may be used (by dogma this has become the right hand).
 * Furthermore, only after the student nails the recitation and also accurately commits the text matter to memory, may permission be given to insribe it. In the Vedic convention, inscription always _follows_ mastery of recitation.
 * Testing of learned material is frequently performed, typically via unscripted dialogue between the learner and the teacher. This enforces real-time analysis of the learner's run-time eval/apply capacity.

Does this training pattern not sound like the way to program a neural network capable of, and indeed empowered by, concurrent use of "multiple intelligences"?
 
**Imagine further**, how it might look in MIT-Scheme's REPL?

	(load "d:\\my-schemes\\aum-ganapatayey-namaha.scm")
	; Pre-loads several procedures into current Global Environment
	; including "read-correctly" "say-precisely" "commit-to-memory" "do-till-eval-apply-mastered"

	(learn-sanskrit book-1.papr book-2.papr book-3.papr)
	; Depends on augmenting itself with pre-loaded procedures 
	; for successful and error-free execution

	(learn-law (attack-books 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))
	; Also depends on augmenting itself with pre-loaded procedures 
	; for successful and error-free execution
	; But it takes a long time to execute, so if you like
	; you may also pre-load "d:\\my-schemes\\regular-coffee-break-generator.scm"

**Eerily**, The very first paragraph of the SICP book reads thus:
 > "We are about to study the idea of a _computational process_. Computational processes are _abstract beings_ (emphasis mine) that inhabit computers. As they evolve, processes manipulate other abstract things called _data_. The evolution of a process is directed by a _pattern_ of _rules_ called a _program_. People create programs to direct processes. In effect, we conjure the spirits of the computer with our spells.
