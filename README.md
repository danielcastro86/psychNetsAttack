# psychNetsAttack

**psychNetsAttack** is an R package for exploring the impact of node deactivation in [Network Properties]. In psychological networks node deactivation might correspond to symptom recovery. Network Theory of Psychopathology suggests that treatments can be improved by acting on central symptoms of the network, since they might help reduce/control the network connectivity.

This packages emulates the effect of symptom recovery after a targeted intervention by deactivating the targeted symptom. Symptoms are targeted and deactivated based on different [Centrality and Controlability Measures].

The package performs a two-step analysis: i) the identification of central symptoms and ii) the exploration of the differential impact of symptoms' deactivation in the network. This can be done through two different [attack types] a normal or a cascade-based attack.

## Example

### Install and Load package from github

``` r
devtools::install_github("danielcastro86/psychNetsAttack", force = T)
library(psychNetsAttack)
```

***Step 1**:* Estimation of the attack effects on the network properties at each symptom deactivation. This is done through the **attack()** function.

This function estimates attack scores for one of the [Attack Results] in combination with a [Centrality and Controlability Measures]. These attack scores consist in a set a [network properties] that are estimated after each symptom deactivation.

#### Perform attack

``` r
scores <- attack(attack = "normal", measure = "strength", graph = inet)
```

***Step 2:*** After the attack scores are estimated we can calculate the [differential impact measures], **attack magnitude** and **attack extent.** This can be done through the function **att.results()** with the [scores]{.underline} from the **attack()** function previously estimated.

#### Attack Results {data-link="Attack Results"}

``` r
results <- att.results(scores)
```

**plot.att()** function can be used to plot the [network properties] after each symptom deactivation.

#### Plot Attack

``` r
plot.att(scores)
```

## Centrality and Controlability Measures

**psychNetsAttack** package estimates several centrality and controlabillity measures:

-   strength centrality

-   degree centrality

-   1-step and 2-step expected influence

-   eigenvector centrality

-   modal control

-   average control

-   1-step and 2-step bridge expected influence

## Attack Types {data-link="Attack Types"}

***Normal attack*** consists in the estimation of central symptoms for the initial complete network.

***Cascade attack*** consists in the estimation of central symptoms after each symptom deactivation. Thus, after each symptom deactivation the central symptoms are estimated again until the last symptom of the network.

For each attack a **random attack** is also estimated where nodes are randomly deactivated.

## Network Properties

After each node deactivation a set of global network properties are estimated. At this time the package estimates:

-   density
-   number of components
-   average path length
-   radius
-   centralization
-   maximum component size
-   cohesion

## Differential Impact Measures

Impact of node deactivation is assessed by computing two measures, attack magnitude and attack extent. These measures are, at the moment, only computed for average path length and the number of components.

**Attack Magnitude** consists of the difference between maximum values and initial values.

**Attack Extent** consists in the proportion of nodes deactivated needed to achieve maximum value.

**Half Nodes** Impact of node deactivation in the network density is assessed by computing the network density after 50% of the nodes have been deactivated.
