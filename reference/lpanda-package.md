# lpanda: Local Political Actor Network Diachronic Analysis Tools

Provides functions to prepare, visualize, and analyse diachronic network
data on local political actors, with a particular focus on the
development of local party systems and identification of actor groups.
Formalizes and automates a continuity diagram method that has been
previously applied in research on Czech local politics, e.g. Bubenicek
and Kubalek (2010, ISSN:1803-8220), Kubalek and Bubenicek (2012,
ISSN:1803-8220), and Cmejrek, Bubenicek, and Copik (2010,
ISBN:978-80-247-3061-5). The package also includes several example
datasets derived from Czech municipal elections, compiled from official
election results, field research, and previously published case studies
on Czech local politics.

## Main functions

The main user-facing functions of `lpanda` are:

- [`prepare_network_data`](https://localpolitics.github.io/lpanda/reference/prepare_network_data.md)() -
  prepares network objects from municipal election results.

- [`plot_continuity`](https://localpolitics.github.io/lpanda/reference/plot_continuity.md)() -
  creates continuity diagrams of local political actors' candidacies
  over time.

Together, these functions facilitate diachronic analysis of local
political actor networks and the evolution of local party systems,
particularly in small municipalities.

## Basic workflow

The typical workflow in `lpanda` centres on *continuity diagrams* that
trace candidacies of local political actors across multiple elections.

To create a continuity diagram of candidacies of local political actors,
it is necessary to prepare election data containing at least unique
names of candidates, unique names of their candidate lists and the years
of the elections. In case of same names, they need to be distinguished,
e.g., by adding numbers after the names of candidates (for example "Doe
Jane (2)" or "Smith John, Jr.") or candidate lists (for example
"Independents 3").

For a basic continuity diagram, you can directly pass the data into the
[`plot_continuity`](https://localpolitics.github.io/lpanda/reference/plot_continuity.md)()
function. However, for deeper analysis, it is recommended to first
process the basic election data using
[`prepare_network_data`](https://localpolitics.github.io/lpanda/reference/prepare_network_data.md)(),
which creates a list of network objects, and only then experiment with
the various parameters offered by
[`plot_continuity`](https://localpolitics.github.io/lpanda/reference/plot_continuity.md)().

## Municipality datasets

The `lpanda` package contains several datasets of municipal election
results that have been the objects of case studies in various
publications focused on Czech local politics. The datasets are compiled
from publicly available data, and in some cases supplemented by findings
from field research. They can be used to reproduce continuity diagrams
published in studies referenced in the documentation of the datasets,
for their deeper analysis, or simply to experiment with the workflow.

List of municipality datasets:

- [`Bublava_SO_cz`](https://localpolitics.github.io/lpanda/reference/Bublava_SO_cz.md)

- [`Cernosice_PZ_cz`](https://localpolitics.github.io/lpanda/reference/Cernosice_PZ_cz.md)

- [`Dasnice_SO_cz`](https://localpolitics.github.io/lpanda/reference/Dasnice_SO_cz.md)

- [`Doubice_DC_cz`](https://localpolitics.github.io/lpanda/reference/Doubice_DC_cz.md)

- [`Horomerice_PZ_cz`](https://localpolitics.github.io/lpanda/reference/Horomerice_PZ_cz.md)

- [`Hradce_CB_cz`](https://localpolitics.github.io/lpanda/reference/Hradce_CB_cz.md)

- [`Jilove_DC_cz`](https://localpolitics.github.io/lpanda/reference/Jilove_DC_cz.md)

- [`Kamenna_CB_cz`](https://localpolitics.github.io/lpanda/reference/Kamenna_CB_cz.md)

- [`Nebanice_CH_cz`](https://localpolitics.github.io/lpanda/reference/Nebanice_CH_cz.md)

- [`Potucky_KV_cz`](https://localpolitics.github.io/lpanda/reference/Potucky_KV_cz.md)

- [`Prameny_CH_cz`](https://localpolitics.github.io/lpanda/reference/Prameny_CH_cz.md)

- [`Roztoky_PZ_cz`](https://localpolitics.github.io/lpanda/reference/Roztoky_PZ_cz.md)

- [`Ustek_LT_cz`](https://localpolitics.github.io/lpanda/reference/Ustek_LT_cz.md)

## References

The continuity diagram approach implemented in `lpanda` builds on
earlier attempts to visualise candidate movements ("defections") between
political parties across elections. Over time, these visualisations
evolved into a more general method for analysing the continuity of local
political actors' candidacies. The method has been progressively
formalised and systematised so that it can be reused across different
case studies.

For an overview of the development of this approach and its applications
in Czech local politics research, see the selected studies below, as
well as the publications listed in the documentation of the individual
datasets.

Bubenicek, V. (2010). *Lokalni modely demokracie v malych obcich CR*
(Local Models of Democracy in Small Municipalities). Dissertation
thesis. Czech University of Life Sciences Prague. \[[Full
text](https://www.pef.czu.cz/dl/46317)\]

Bubenicek, V., & Kubalek, M. (2010). Konfliktni linie v malych obcich
(Cleavages in Small Municipalities). *Acta Politologica*, 2(3), 30-45.
\[[Full
text](https://acpo.fsv.cuni.cz/ACPONEN-48-version1-acpo_2010_03_04.pdf)\]

Cmejrek, J., Bubenicek, V., & Copik, J. (2010). *Demokracie v lokalnim
politickem prostoru* (Democracy in Local Political Area). Prague: Grada.
\[[Publisher
link](https://www.grada.cz/demokracie-v-lokalnim-politickem-prostoru-5880/)\]

Kubalek, M., & Bubenicek, V. (2012). Charakter lokalni politiky v
suburbannim politickem prostoru (The Nature of Local Politics in
Suburban Political Space). *Acta Politologica*, 4(3), 284-305. \[[Full
text](https://acpo.fsv.cuni.cz/ACPONEN-50-version1-acpo_2012_03_05.pdf)\]

Kotaskova, S. K. (2016). Cleavages and political pluralism in the small
municipality in Czech Republic. *Global Journal of Business, Economics
and Management: Current Issues*, 5(2), 63-69.

Hornek, J. (2022). *Zhroucene obce v Ceske republice* (Failed
Municipalities in the Czech Republic). Dissertation thesis. Charles
University. \[[Full
text](https://dspace.cuni.cz/handle/20.500.11956/177784?locale-attribute=en)\]

Krpalkova, S. (2024). *Permanentni opakovani komunalnich voleb:
zablokovane obce?* (Permanent Repetition of Municipal Election: Blocked
Municipalities?). Dissertation thesis. Charles University. \[[Full
text](https://dspace.cuni.cz/handle/20.500.11956/191991?locale-attribute=en)\]

## See also

Useful links:

- <https://localpolitics.github.io/lpanda/>

- <https://CRAN.R-project.org/package=lpanda>

- <https://github.com/localpolitics/lpanda>

- Report bugs at <https://github.com/localpolitics/lpanda/issues>

## Author

**Maintainer**: Vaclav Bubenicek <bubenicek@pef.czu.cz>
([ORCID](https://orcid.org/0000-0002-0906-0750)) (Czech University of
Life Sciences Prague (CZU)) \[copyright holder\]
