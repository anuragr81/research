# BEJEAP referee reports, second round (DGBEJEAP.2026.0180, rejected 31 August 2026)

Source: the reviewers' comments to the author, pasted verbatim by the author on 19 October 2026.
Reviewer 2's report arrived in two pastes on 19 October 2026; the second restarted at Comment 4,
and Comment 4 below is taken whole from it. Text below is reproduced as pasted.

---

Reviewer(s)' Comments to Author:

Reviewer: 1

Comments to the Author
The paper studies a fixed-slot status competition between one high-income participant and Q >= 1 low-income participants. Agents differ only in initial income and in random realizations of raw talent and social capital. Before the contest, each agent chooses whether to pay a participation cost nu in order to access the social-capital component of the contest score. The winner obtains the high-income position, while all other participants receive the low-income outcome. The main mechanism is that participation costs can generate sharp regime changes: when the cost is sufficiently low, both high- and low-income agents invest; when the cost is intermediate, only the high-income agent invests; and when the cost is sufficiently high, participation may collapse. The paper interprets the intermediate regime as an “elite-only” equilibrium and argues that it generates a mobility ceiling for low-income agents.

The revised version is clearer than the previous one. In particular, the paper no longer presents the mechanism as Spence-style signaling, and it now more explicitly limits the intended interpretation to fixed-slot positional competitions. These are useful changes. However, I do not think the revision resolves the main concerns. The model remains too stylized to support the broader education-policy and inequality-dynamics conclusions, and the new general-Q extension appears to introduce a serious technical problem in the Nash equilibrium analysis.

Main comments

1. The general-Q equilibrium analysis appears to use the wrong deviations.

The main concern with the new general-Q extension is that the paper treats the Q low-income participants as if they were a single representative player. This is not the standard Nash logic for a finite-player game. In a Nash equilibrium, one low-income participant deviates while the other Q-1 low-income participants keep their strategies fixed. Thus, in the proposed exclusive-participation profile, a deviating low-income participant who invests does not move the game to full participation; she invests while the other low-income participants still do not. Similarly, in the full-participation profile, a deviating low-income participant who withdraws still faces Q investing opponents. This matters for the equilibrium conditions. The paper appears to compare payoffs across aggregate regimes, rather than checking individual unilateral deviations. As a result, the claimed general-Q equilibrium classification, the participation thresholds, the comparative statics in Q, and the mobility-ceiling result may not follow from the stated finite-player game. The authors should either redo the analysis using standard unilateral deviations, allowing for profiles with any number of investing low-income participants, or explicitly reformulate the model as a two-player game between the high-income participant and a collective low-income group.

2. The reach into education policy remains limited.

The revision partially addresses my earlier concern that the model is too stylized for education policy or inequality dynamics: the author now restricts the interpretation to fixed-slot positional competitions, excluding settings in which education raises productivity across jobs or losing investors retain human-capital gains. This is a useful clarification, but it does not remove the underlying limitation. The model remains a zero-sum status contest. Social capital affects only the probability of winning a fixed position; it does not enter a production function, raise aggregate output, benefit losing investors, or generate intergenerational dynamics. This may suit narrow positional competitions but is a strong assumption for a paper drawing conclusions about education policy and inequality.

The issue is therefore one of scope. The title, abstract, introduction, and policy section still draw broad conclusions about mobility, education policy, and class barriers. The model supports claims about fixed-prize, fixed-slot competitions in which social capital affects rankings but not productivity; without further structure it cannot support claims about education as human capital, aggregate output, intergenerational mobility, or general labor-market inequality. These claims should be narrowed accordingly.

3. Robustness beyond uniform distributions.

The new paragraph asserting that the key qualitative results extend beyond the uniform case states an intention rather than a theorem. The closed forms for p_1, nu*, and the ceiling mu/[2(1-mu)] rely on the uniform specification, and the claimed extension of the binary equilibrium structure and comparative statics to symmetric distributions is not proven. The concern is sharper given point 1: the correct unilateral-deviation probabilities involve objects such as int F_X(x)G(x)^{Q-1} dF_X(x), whose behavior may depend nontrivially on the distributions of r and s. The author should either provide precise assumptions and a formal result, or present the uniform model as a tractable example rather than a generic theorem.

4. Contribution to the literature.

The revision engages more directly with related work and distinguishes its threshold-participation mechanism from models of continuous investment intensity. The substantive contribution nonetheless appears modest: the core observation is that a threshold cost in a fixed-prize status contest can generate participation and non-participation regimes. The paper should explain why this is not already captured by existing models of contests with entry costs, positional education, or status competition. The “mobility ceiling” is also less general than claimed. It is an asymptotic expression from the exclusive-participation probability formula, and it is not clear that it reflects a robust economic principle rather than a property of the uniform specification. The paper would be stronger presenting the ceiling as a model-specific implication and centering the contribution on the interaction between threshold costs and positional competition, rather than as a broad structural limit on mobility policy.

Additional comments

1. The revision addresses the misleading use of “signaling” from the previous version. This is a useful improvement.

2. The term “stable Nash equilibrium” is used repeatedly, but stability is not defined. If the paper means pure-strategy Nash equilibrium, it should say so. If it means some refinement or dynamic stability notion, that notion should be defined.

3. The claim that exactly two stable Nash equilibria arise should be qualified. The model section also discusses full withdrawal as a Nash equilibrium for sufficiently high participation costs. Thus the paper should either restrict the claim to the relevant parameter region or present the equilibrium regimes as full participation, exclusive participation, and full withdrawal.

4. The “mobility ceiling” claim should be stated more carefully. If the ceiling refers to the bound within the exclusive-participation regime, then cost reductions do not affect it as long as the economy remains in that regime. But a sufficiently large reduction in participation costs may move the economy from exclusive participation to full participation, in which case the relevant mobility bound changes. The abstract and policy discussion should use the term consistently.

5. The discussion of inequality should distinguish prize effects from affordability effects. Holding y_1 and nu fixed, an increase in y_2 raises the prize u(y_2)-u(y_1) and can strengthen low-income incentives to invest. The force that prices out low-income participants is instead the budget or utility-cost burden of paying nu out of low income. These channels should not be conflated.

6. The notation Phi(Q) is inconsistent. It is defined in the text as a probability surplus scaled by the utility gap and the discount factor, but Proposition 2 and Table 3 treat it as an unscaled probability surplus. The paper should either redefine Phi(Q) as a pure probability object or carry the utility and discount factors throughout.

7. Some comparative statics with respect to mu and 1-mu appear misstated. Under the paper’s own expression for A, increasing mu lowers A, while increasing the social-capital weight 1-mu raises A. The text sometimes says that the participation threshold is decreasing in the social-capital weight, which appears to be the opposite of what the formula implies.

8. The figures are improved, but the paper remains verbose. Many derivations could be moved to the appendix, and the main text could focus on the economic mechanism, the equilibrium conditions, and the interpretation of the results.

Reviewer: 2

Comments to the Author
Report on “Participation Costs, Social Capital, and the Limits of Mobility Policy - A Model of Status Competition under Income Inequality”, by Anurag Srivastava, submitted to The B.E. Journal of Economic Analysis and Policy

Summary

This article studies a contest in which the winning probability depends on raw talent r_{i} and the social capital s_{i} of a participant i and in which acquiring social capital is associated with a fixed access cost \overline{v} that has to be paid in period 1. The winner of the contest earns income y_{2} from period 2 onward, while losers earn income y_{1} (< y_{2}). The winner of the contest is the participant with the largest score \mu r_{i}+(1-\mu)s_{i}, where 0<\mu<1 measures the importance of raw talent relative to social capital. s_{i}=0 holds for participants that do not invest in social capital, while s_{i} is ex ante unknown and drawn from a uniform distribution for those who do invest. Also r_{i} is drawn from a uniform distribution and is assumed to be unknown at the time of investment. There is an incumbent who earns y_{2} in the initial period and is therefore richer than the other participants, who earn y_{1}. Households cannot save or borrow and the concavity of the utility function implies that investment in social capital is cheaper (in utility terms) for the incumbent.

The article analyzes the case in which there are Q “poor” participants (initialy earning y_{1}) and studies Nash equilibria in which all poor participants adopt symmetric investment strategies: in an “exclusive participation” (EP) equilibrium, only the incumbent invests in social capital, in a “full participation” (FP) equilibrium, all participants invest, and in a “full withdrawal” (FW) equilibrium, no one invests. An “exclusive participation” (EP) equilibrium with two participants in which the incumbent does not invest, while the other poorer participant invests is also analyzed. This equilibrium, however, does not exist if utility is concave in income.

It is shown that the full participation equilibrium is only possible for sufficiently low access costs \overline{v}, while the full withdrawal equilibrium results if \overline{v} is sufficiently large. For intermediate values of \overline{v}, the exclusive participation equilibrium results in which only the incumbent invests.

A “participation threshold” \overline{v}^{*}is defined as the threshold of \overline{v} for which poor participants are indifferent between investing and not investing, given that the incumbent invests. \overline{v}^{*} is then shown to fall in the number Q of low income participants and in \mu  (the importance of talent relative to social capital for winning). Rises in Q or \mu  therefore make an exclusive participation equilibrium more likely relative to a full participation equilibrium as they lower the winning probability in the case of investment.

The paper studies how the probability that the incumbent is replaced by a poor participant depends on the number Q of poor participants. It is shown that in an exclusive participation equilibrium, this probability reaches an upper bound (“mobility ceiling”) when Q goes to infinity and that this upper bound rises in the talent weight \mu . In a full participation equilibrium, an increase in Q towards infinity instead pushes the probability that the incumbent is replaced towards one.

The article then goes on to discuss the effects of policies that try to manipulate the access costs (\overline{v}), the number of participants Q of the context, and the weight put on talent in recruiting (\mu ).

General Assessment

In my view, the article discusses an interesting question, but the analysis lacks clarity and it is often difficult to follow the arguments. In the following, I address several issues in more detail.

Comment 1: Asymmetric equilibria

The analyses focuses on symmetric equilibria in which all poor households either invest or do not invest in social capital. However, there also seem to be asymmetric equilibria in which only some poor households invest, while others do not. Consider the simplest case where \mu=0 so that only participants who have invested in social capital can win the contest. The incentives to invest in social capital then clearly fall if more other participants have invested, as the probability of winning then declines. There should therefore be Nash equilibria in which only part of the poor households invest. Also in the more general case with positive \mu , the following results will probably hold: more participants will invest if the price of the contest, y_{2}-y_{1} increases, if the investment costs \overline{v} fall, and if social capital becomes more important (\mu  falls), as the latter rises the winning probability for social capital investors. If the number Q of participants increases, the effect on the number of participants investing in social capital will probably depend on the size of \mu : if \mu=0, all gains y_{2}-y_{1}of the contest will be divided among the investing participants and their equilibrium number will be independent of the number Q of total participants. If, however, \mu>0 holds and participants without social capital s_{i} but with a lot of raw talent r_{i} have a chance of winning, an increase in Q should reduce the size of the pie that is distributed among the investing participants, implying that less participants will invest in social capital in equilibrium.

Allowing for such asymmetric equilibria would change the current analysis in the following way: for an exclusive participation equilibrium to exist, it must then be verified that no poor household has an incentive to deviate and to become the only poor investor, and not simply that poor households do not want to jointly deviate to a full participation equilibrium (as done in equation 8). There would then be several intermediate equilibria between the exclusive participation and the full participation equilibrium (each involving different “participation thresholds”), making the effects of changes in various policy parameters more smooth.

Applying the same logic of asymmetric equilibria, I wonder whether it might also be possible to also obtain an exclusive participation (EP) equilibrium (in which one poor household invests while the incumbent does not invest) even if utility is concave so that the incumbent has a cost advantage in investing.

Comment 2: The effect of inequality on status expenditure

It is mentioned in the abstract that “wider income gaps simultaneously strengthen the incumbent's investment incentive and price out challengers, reconciling rising inequality with rising status expenditure”. I do not see where this statement is proven in the paper and what it exactly means. While it seems intuitive that an increase in y_{2}-y_{1}increases investment incentives by increasing the price of the contest, this should be the case not only for the incumbent, but also for poor participants. If insteady_{2}-y_{1}increases because y_{1}decreases, poor households might stop investing as they cannot afford to do so anymore, leading instead to a fall in total investment. Or is the argument that an increase in y_{2}increases the probability that an incumbent who was initially not investing now starts to invest? This should be clarified.

It also seems to me that talking about the effect of inequality on status expenditure (social capital) is a bit misleading when the degree of inequality y_{2}-y_{1} directly corresponds to the price that can be won in the contest. Some of the related literature that is cited in the introduction instead analyzes how ex ante inequality in endowments affects the incentives to invest in status, taking the reward obtained from status as given. In the latter case, it seems to me that the standard result that larger inequality reduces total status expenditures should also hold in the present setting.

Comment 3: policy implications

The paper applies the model to several real world examples and studies policy implications. This is, however, done without first specifying a clear policy objective. In the model, the contest is a zero-sum game in which the size of the price neither depends on raw talent, nor on the social capital of the winner. As expenditures on social capital are costly but do not increase the size of the price, they are socially inefficient. One policy objective could therefore be to study the policy that minimizes the total expenditures on social capital. Policies that would achieve zero social capital expenditures would for example be to set the talent weight \mu  equal to 1 (so that social capital would no longer be rewarded in the contest) or to make investing in social capital so costly (by increasing \overline{v} sufficiently) that no one would invest in it anymore. None of these possibilities is addressed in the paper, and it even seems that the author considers a broader investment in social capital to be a positive thing. This should be motivated.

Another policy objective might be to select the winner who has the largest raw talent. Also in this case, setting \mu=1 or \overline{v} sufficiently high would lead to the optimal solution. It should therefore be explained why these solutions are not considered optimal or attainable. As it seems reasonable to assume that it is impossible to set \mu=1 or \overline{v} sufficiently high in the real world, analyzing other policies can still be interesting. But this should be done with a clear benchmark in mind, like for example the total spending on social capital, or the waste of raw talent if the winner is not the participant with the largest raw talent. Such analyses would of course be more interesting in a setting with asymmetric equilibria (see Comment 1).

Comment 4: mobility ceilings and the limits to growth

The articles puts a lot of emphasis on the fact that in the case where only the incumbent invests in social capital, the probability with which she is replaced reaches an upper bound of \frac{\mu}{2\left(1-\mu\right)} if the number Q of poor participants goes to infinity. The explanation for this is the following: if for example \mu=\frac{1}{2} so that raw talent only contributes to one half of the final contest score, the incumbent who randomly draws both raw talent and social capital from uniform distributions with support \left[0,1\right] has an average score of \frac{1}{2} and wins with probability \frac{1}{2} against a poor participant who has the maximal raw talent of 1 (as he is selected from an infinite number of other poor participants who also draw their talents from a uniform distribution) and therefore always has a score of \frac{1}{2}. This result therefore seems to be rather driven by the underlying assumptions than to constitute some major insight. Moreover, increasing Q to infinity when \mu>0 holds is inconsistent with the existence of an equilibrium in which the incumbent invests in social status, so that analyzing the case of a large Q becomes meaningless (the same holds true in the full participation equilibrium). Studying the case in which the number of contest participants continues to increase while the price that can be won and the investment costs remain the same also seems to be quite specific and not necessarily in line with real world examples. Talking about the limits to growth in such a zero-sum contest with a fixed price also seems quite a stretch.

Comment 5: Modeling assumptions

At some points, the model seems overly complex, while it could be more general at others:

• It might be simpler to just study a static one-shot model instead of the dynamic setting introduced on p.8.. In order to understand the main mechanisms, it might also be useful to first discuss the case of symmetric participants, and to only introduce asymmetries later on. Instead of assuming that the incumbent has a higher income in the first period, it could for example be assumed that the incumbent simply has lower investment costs (without the concavity of the utility function playing a role).

• The assumption of financially constrained households also does not seem to be very crucial as the main insights come from the incentives to invest. So, it might be clearer to initially consider a setting without financial constraints and to then maybe look at financial constraints in an extension.

• In order to simplify the analysis further, it would also be possible to analyze a reduced-form contest function in which the winning probabilities are not micro-founded.

• It is not clear to me why the assumption \mu<\frac{1}{2} is needed. Wouldn't the main results remain the same if \mu>\frac{1}{2} were allowed?

Comment 6: Exposition

The analysis would become clearer if the main results were stated in precise propositions, followed by proofs (right now, the only proposition is about an equilibrium that does not exist under standard assumptions). These propositions should state how, when exogenous parameters fall into a certain range, the change of a particular exogenous variable affects a particular endogenous variable. Currently, there is often only a loose verbal discussion of possible effects (sometimes changing several variables at the same time), without checking for which parameter values a particular equilibrium exists and when exactly the equilibrium switches. A further problem is that the verbal discussions (also of the related literature) are often imprecise and that it is at times difficult to understand the link between the mathematics and the verbal analysis. Some examples:

- p.5: “a participant abstains and saves instead”, but saving is not allowed in the model

- p.6: mathematical parameters (p_{1}...) are used without first explaining their meaning

- p.9 middle: the mathematical derivations should be better explained

- p.10, Proposition 1: “the marginal utility cost of \bar{v} is higher”; what are marginal utility costs?

- p.11 top half: “... the probability surplus from being the sole investor narrows, and the IP window must be correspondingly tighter so sustain the equilibrium. At \mu\rightarrow0 (pure raw-talent competition)...”. This should be made clearer and some mistakes corrected

- p.14 middle: “A uniform upward shift in both incomes that leaves the ratio constant has no effect on the participation threshold” is not correct given equation 20

- p.14, last paragraph: seems to contain a mistake

- p.15 middle: “the corresponding minimum income ratio...” how is this defined? Where does it appear in the table?

- p.16 middle: why does the high income participant “free ride on raw talent”? Why does the poor participant “over-invest”?

- p.17 middle: “Access expansion without competition redesign reinforces elite-only equilibria”. Why is this bad if the incumbent is replaced with a larger probability?

- p.17 bottom: “it raises competitive intensity” What does this mean?

-p.18 middle: “as if it also removes budget constraints”. What does it mean?

- p.18 bottom: “reference-dependent equilibrium”. What is this?

- p.19 top: “is aligned with the Sutton trust's finding that the Russell Group share of disadvantaged students declined...”. Doesn't the model predict a higher probability that poor participants win when Q increases?

- p.19 bottom: “move the budget constraint from binding to non-binding”. What matters are the incentives to invest, not only whether a household is financially constrained...

- p.18: the conclusion should be much shorter
