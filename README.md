# psychNetsAttack

psychNetsAttack is an R package for exploring the impact of node deactivation in network global properties.

The package performs a two-step analysis: i) the identification of central symptoms nad ii) the exploration of the differential impact of symptoms' deactivation in the network.

## Example
(not working, only illustrative of the procedure)

### Load package from github

```
devtools::install_github("danielcastro86/psychNetsAttack", force = T)
library(psychNetsAttack)
```

### Perform attack 

```
att.scores <- attack(attack = c("normal", "cascade"), 
                    measure = c("strength", "bridge strength", "average control", "modal control", "degree", "eigenvector", "bridge expected influence 1-step", "bridge expected influence 2-step", "expected influence 1-step", "expected influence 2-step"), 
                    graph = igraphnetwork)
```

### Plot Attack

```
plot.att(att.scores)
```

### Attack Results

```
results <- att.results(att.scores)
```

## Centrality and Controlability measures

psychNetsAttack package estimates several centrality and controlabillity measures:
  
  - strength centrality
  - degree centrality
  - 1-step and 2-step expected influence
  - eigencvector centrality
  - modal control
  - average control
  - 1-step and 2-step bridge expected influence
  

## Attack Types

Two attack types can be used in the psychNetsAttack package. 

**Normal attack**, where symptoms were deactivated according to their original centrality. 

**Cascade attack**, where symptoms are deactivated in their decreasing order according to their centrality, which is iteratively calculated at every symptom removal

For each attack a **random attack** is also estimated where nodes are randomly deactivated.


## Network Properties

After each node deactivation a set of global network properties are estimated. At this time the package estimates:

  - density
  - number of components
  - average path length
  - radius
  - centralization
  - maximum component size
  - cohesion
  
  
## Differential impact measures

Impact of node deactivation is assessed by computing two measures, attack magnitude and attack extent. These measures are, at the moment, only computed for average path length and the number of components.

**Attack Magnitude** consists of the difference between maximum values and initial values.

**Attack Extent** consists in the proportion of nodes deactivated needed to achieve maximum value.

**Half Nodes** Impact of node deactivation in the network density is assessed by computing the network density after 50% of the nodes have been deactivated.

 
