nomnoml::nomnoml("#import: my-common-styles.nomnoml
#stroke: black
#.blob: fill=#DBDBDB visual=italic
#direction: down
#edges: rounded
#arrowSize: 0.6
#bendSize: 0.3
#fillArrows: true

  [129 references imported for screening]->[94 studies screened against title and abstract]

  [129 references imported for screening]-->[<blob> 35 duplicates removed]

  [94 studies screened against title and abstract]->[23 studies assessed for full-text eligibility]
  [94 studies screened against title and abstract]-->[<blob> 71 studies excluded]

[23 studies assessed for full-text eligibility] -> [15 studies included]

 [23 studies assessed for full-text eligibility] --> [<blob> 6 studies excluded | 4 wrong intervention
                                                      1 wrong comparator
                                                      1 wrong study design]
 [23 studies assessed for full-text eligibility] --> [<blob> 2 studies awaiting classification]", 
                 png = here::here("figure_one.png"))
