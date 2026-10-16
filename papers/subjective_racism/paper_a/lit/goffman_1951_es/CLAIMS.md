# Goffman (1951), Spanish translation, claims

Evidence tag [F]: the authorised Spanish translation by Servando Ortoll,
"Símbolos de status de clase", all 16 PDF pages, was read in full on 16
October 2026 from the text layer, with page images of PDF pages 6 and 13. The
English original, *British Journal of Sociology* 2(4), 1951, pp. 294–304, is
**not read** (GF-D1). Pages below are the translation's PDF pages, which equal
its printed pages. Quotations are the translation's Spanish, verbatim. Each
English rendering in the last column is **our translation** of the Spanish,
and so a translation of a translation, not Goffman's English.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| GF-Q1 | 2 | "Estos son las señales que seleccionan para un individuo el status que se le habrá de imputar y la manera en que otros habrán de tratarlo." | Our translation: "These are the signs that select for an individual the status to be imputed to him and the way others are to treat him." A status symbol is a sign read by others, and the reading sets treatment. This is A1's premise of inference followed by treatment |
| GF-Q2 | 2 | "Los símbolos de status visiblemente dividen al mundo social en categorías de personas, ayudando de ese modo a mantener solidaridad dentro de una categoría y hostilidad entre categorías diferentes." | Our translation: "Status symbols visibly divide the social world into categories of persons, thereby helping to maintain solidarity within a category and hostility between different categories." Solidarity within and hostility between categories is asserted as a function of symbols, not derived from preferences |
| GF-Q3 | 3 | "Cualquier señal que provea evidencia confiable sobre la posición de quien la haga [...] puede ser llamada una prueba de status. Este ensayo se ocupa de las presiones ejercidas sobre la conducta como resultado del hecho de que un símbolo de status no siempre es una buena prueba de status." | Our translation: "Any sign that provides reliable evidence of the position of whoever makes it [...] may be called a status test. This essay concerns the pressures exerted on conduct as a result of the fact that a status symbol is not always a good status test." The distinction between a symbol and a reliable test, formalised in our reading as `IsPrueba` |
| GF-Q4 | 3 | "Es siempre posible, por ello, que los símbolos lleguen a ser empleados de manera "fraudulenta", es decir, para indicar un status que el aspirante de hecho no posee." | Our translation: "It is always possible, therefore, that symbols come to be employed 'fraudulently', that is, to indicate a status that the claimant does not in fact possess." Passing. Misrepresentation is possible because the symbol is distinct from what it signifies |
| GF-Q5 | 5 | "los símbolos de clase no sirven tanto para representar o falsear la posición del individuo, sino más bien para encauzar la opinión de otras personas, sobre éste." | Our translation: "class symbols serve not so much to represent or misrepresent the individual's position as to channel other people's opinion of him." For class, unlike occupation, fraud holds only in a weakened sense, since class position is itself a matter of others' judgement within a margin of doubt |
| GF-Q6 | 6 | "obviamente implican que el propietario tiene al menos tanto dinero como los símbolos pueden conseguir en el mercado abierto." | Our translation: "obviously imply that the owner has at least as much money as the symbols can fetch on the open market." The intrinsic restriction, a symbol of wealth that certifies wealth by its price. Formalised as `intrinsic_is_prueba` |
| GF-Q7 | 9 | "la asociación personal con individuos de status elevado se usa como símbolo de status." | Our translation: "personal association with individuals of high status is used as a status symbol." Contact is itself a class symbol, a reason to seek high-status contact that differs from A1's homophily |
| GF-Q8 | 9 | "El asistir al teatro puede ser utilizado así como un símbolo de status mientras que una visita a la sala cinematográfica, en general, no se puede usar de esta manera." | Our translation: "Attending the theatre can thus be used as a status symbol, whereas a visit to the cinema, in general, cannot be used in this way." Attendance at a venue as a class symbol, protected by the venue's limited audience |
| GF-Q9 | 10 | "Estas conductas suponen cuestiones de etiqueta, vestido, comportamiento, ademán, entonación, dialecto, vocabulario, pequeños movimientos corporales" | Our translation: "These conducts involve matters of etiquette, dress, deportment, gesture, intonation, dialect, vocabulary, small bodily movements". Speech, intonation and dialect are named among the symbols of social style, protected by the restriction of socialisation |
| GF-Q10 | 10 | "se otorga un valor simbólico a la diferencia perceptible entre un acto desempeñado maquinalmente bajo la guía invisible de la familiaridad y el hábito, y el mismo acto o una imitación del mismo, representado con atención consciente al detalle" | Our translation: "a symbolic value is given to the perceptible difference between an act performed mechanically under the invisible guidance of familiarity and habit, and the same act, or an imitation of it, performed with conscious attention to detail". An imitated style can be told from a habitual one, which limits passing by style |
| GF-Q11 | 12 | "no existe un sólo modo de restricción que pueda resistir demasiadas contingencias, ni que haya restricción alguna que no sea regular y sistemáticamente sorteada de alguna manera." | Our translation: "there is no single mode of restriction that can withstand too many contingencies, nor any restriction that is not regularly and systematically circumvented in some way." Every restriction is evaded, so passing is routine |
| GF-Q12 | 12 | "El uso de características heredadas como símbolos de status se halla típicamente, por supuesto, en una sociedad de castas y no de clases." | Our translation: "The use of inherited characteristics as status symbols is typically found, of course, in a caste society and not a class society." In Goffman's terms an inherited group marker is a caste symbol, not a class symbol |
| GF-Q13 | 13 | "Parece que la eficacia de un tipo de restricción actúa como un control sobre el fracaso de otro." | Our translation: "It seems that the efficacy of one kind of restriction acts as a check on the failure of another." Formalised in our reading as `conjunctive_restrictions` |
| GF-Q14 | 13 | "incrementando la tendencia para que las señales que simbolizan la posición, tomen el rol de conferirlo." | Our translation: "increasing the tendency for the signs that symbolise position to take on the role of conferring it." Under class movement the symbol confers the standing it once indicated |
| GF-Q15 | 14 | "depender cada vez más en símbolos que no suponen un desembolso actual, mientras que al mismo tiempo su asociación con estos símbolos reduce el valor de estas señales ante los ojos de los demás." | Our translation: "to rely more and more on symbols that involve no current outlay, while at the same time their association with these symbols reduces the value of these signs in the eyes of others." The value of a symbol falls with the standing of those who display it. Formalised in our reading as `association_lowers_value` |

## Lean results

`lean/mathlib/Goffman1951es.lean`. The essay states no model. The file
formalises our reading of three structures in it.

| Theorem | What it checks |
|---|---|
| `Goffman1951es.intrinsic_is_prueba` | The intrinsic restriction. If display requires paying the market price `p` out of wealth, the symbol is a *prueba* of wealth `p` |
| `Goffman1951es.prueba_mean_ge` | The mean wealth of the displayers of a *prueba* of `p` is at least `p` |
| `Goffman1951es.association_lowers_value` | New displayers below the users' mean standing lower the symbol's value, read as the mean standing of all its displayers |
| `Goffman1951es.conjunctive_restrictions` | A pretender who must evade two restrictions succeeds no more often than against either alone |
| `Goffman1951es.control_counterfeit` | Control. Without the intrinsic restriction a counterfeit at a lower price makes the symbol no *prueba* |
| `Goffman1951es.control_association_from_above` | Control. New displayers at or above the mean do not lower the symbol's value |
| `Goffman1951es.control_alternative_restrictions` | Control. If evading any one restriction suffices, a second restriction weakens the symbol rather than protecting it |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| GF-D1 | The version read is the Spanish translation by Servando Ortoll, published by the Universidad de Sonora "con el permiso escrito de la Blackwell Publishing Ltd." (PDF p. 1, translator's note). It is cited as the translation, never as the English original | The English original is not read. Every English rendering here is ours, of the Spanish, and cannot stand as Goffman's wording |
| GF-D2 | The date of the translation is not stated in the text. The PDF metadata give the author as "Servando Ortoll, PhD" and a creation date of 28 February 2008. The file came from `sociologia.unison.mx/docs/publicaciones/cuadernodetrabajo/4simbolosdeestatusdeclase.pdf` | The bib entry carries the original's year with a note that the translation is the version read |
| GF-D3 | The first item of each list in sections II and III is numbered "(I)" and the rest "(2)", "(3)", and so on (PDF pp. 6, 13). Read as (1) | The page image of PDF p. 6 shows "(I)" in the PDF itself, so it is not an extraction error |
| GF-D4 | Section I has no heading in the translation, while sections II and III do | Read as an omission of the heading. Nothing rests on it |
| GF-D5 | "símbolos de ocupación" and "símbolos de profesión" (PDF p. 4) are read as one category, occupational symbols | The paragraph introduces the first term and then describes "dos tipos principales de símbolos de profesión" |
| GF-D6 | "Bretaña" (PDF p. 12) is read as Britain, and "nuestra sociedad inglesa" (PDF p. 5) as British society | The examples, the public schools and the appearance of hands and height, are British. Whether the original says "English" or "British" cannot be checked without it |
| GF-D7 | "intrinsicos" (PDF p. 8) is read as "intrínsecos" | Spelling variant in the translation |
| GF-D8 | The six restrictions are moral, intrinsic, natural, socialisation, refinement and organic, numbered (I) to (6) on PDF pp. 6 to 12 | PDF p. 12, "Seis dispositivos generales para restringir el abuso de símbolos de clase han sido esbozados" |
