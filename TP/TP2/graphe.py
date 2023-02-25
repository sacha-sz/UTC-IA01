import graphviz


def graphe_complet(dot1, i, j):
    global node_index

    parent = node_index
    dot1.node(str(node_index), f'({i} {j})')

    node_index += 1

    if i == 0:
        return dot1

    if i >= 3:
        enfant1 = node_index
        dot1 = graphe_complet(dot1, i-3, 1-j)

        enfant2 = node_index
        dot1 = graphe_complet(dot1, i-2, 1-j)

        enfant3 = node_index
        dot1 = graphe_complet(dot1, i-1, 1-j)

        dot1.edge(str(parent), str(enfant1))
        dot1.edge(str(parent), str(enfant2))
        dot1.edge(str(parent), str(enfant3))

    elif i == 2:
        enfant1 = node_index
        dot1 = graphe_complet(dot1, i-2, 1-j)

        enfant2 = node_index
        dot1 = graphe_complet(dot1, i-1, 1-j)

        dot1.edge(str(parent), str(enfant1))
        dot1.edge(str(parent), str(enfant2))

    else:  # i == 1
        enfant1 = node_index
        dot1 = graphe_complet(dot1, i-1, 1-j)

        dot1.edge(str(parent), str(enfant1))

    return dot1


def graphe_tronque(dot1, i, j):
    global node_index
    global nodes_already_seen

    parent = node_index
    dot1.node(str(node_index), f'({i} {j})')

    node_index += 1

    if i == 0:
        return dot1

    if (i, j) in nodes_already_seen:
        dot1.node(str(node_index), f'...')
        dot1.edge(str(parent), str(node_index))
        node_index += 1
        return dot1
    else:
        nodes_already_seen.add((i, j))

    if i >= 3:
        enfant1 = node_index
        dot1 = graphe_tronque(dot1, i-3, 1-j)

        enfant2 = node_index
        dot1 = graphe_tronque(dot1, i-2, 1-j)

        enfant3 = node_index
        dot1 = graphe_tronque(dot1, i-1, 1-j)

        dot1.edge(str(parent), str(enfant1))
        dot1.edge(str(parent), str(enfant2))
        dot1.edge(str(parent), str(enfant3))

    elif i == 2:
        enfant1 = node_index
        dot1 = graphe_tronque(dot1, i-2, 1-j)

        enfant2 = node_index
        dot1 = graphe_tronque(dot1, i-1, 1-j)

        dot1.edge(str(parent), str(enfant1))
        dot1.edge(str(parent), str(enfant2))

    else:  # i == 1
        enfant1 = node_index
        dot1 = graphe_tronque(dot1, i-1, 1-j)

        dot1.edge(str(parent), str(enfant1))

    return dot1


def main():
    global node_index
    global nodes_already_seen

    node_index = 1
    nodes_already_seen = set()

    dot = graphviz.Digraph('arbre-de-recherche', comment='Arbre de recherche', format='png')

    joueur = input('Joueur départ (0 ou 1) : ')
    if joueur == '0':
        joueur = 0
    else:
        joueur = 1

    nb_allumettes = int(input('Nombre d\'allumettes départ : '))
    if nb_allumettes < 0:
        nb_allumettes = 16

    if input('Graphe complet (o/n) : ') == 'o':
        print(f'Graphe complet, joueur depart : {joueur}, allumettes depart : {nb_allumettes}')
        graphe_complet(dot, nb_allumettes, joueur)
    else:
        print(f'Graphe tronqué, joueur depart : {joueur}, allumettes depart : {nb_allumettes}')
        graphe_tronque(dot, nb_allumettes, joueur)

    dot.render(directory='render', view=True)


if __name__ == '__main__':
    main()
