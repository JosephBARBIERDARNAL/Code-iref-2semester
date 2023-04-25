def draw_graph(graph, layout="random", node_size=8000, figsize=[100, 60],
               node_color="teal", edge_color="silver"):
    
    # Layout
    if layout=="random":
        pos = nx.random_layout(graph)
    elif layout=="circular":
        pos = nx.circular_layout(graph)
    elif layout=="spring":
        pos = nx.spring_layout(graph)
    else:
        return "Invalid layout. Try: 'random', 'circular' or 'spring'."

    # Nodes
    if node_size=="node_degree":
        node_size = [d*1000 for n, d in graph.degree]
    else:
        try:
            node_size = float(node_size)
        except ValueError:
            node_size = 8000
            print(f"Node size put to {node_size}")
    nx.draw_networkx_nodes(graph, pos, node_color=node_color, node_size=node_size)

    # Edges
    nx.draw_networkx_edges(graph, pos, edge_color=edge_color, width=3.0, alpha=0.5)

    # Parameters
    plt.rcParams['figure.figsize'] = figsize
    plt.axis('off')
    plt.show()