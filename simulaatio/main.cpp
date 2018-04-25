#include <iostream>
#include <random>
#include <iterator>
#include <fstream>

#include <boost/config.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphviz.hpp>

template <typename RandomGenerator = std::default_random_engine>
struct random_selector {
   public:
      random_selector(RandomGenerator g = RandomGenerator(std::random_device("/dev/urandom")()))
            : gen(g) {}

      template <typename Iter>
      Iter select(Iter start, Iter end) {
         std::uniform_int_distribution<> dist(0, std::distance(start, end) - 1);
         std::advance(start, dist(gen));
         return start;
      }
   
   private:
      RandomGenerator gen;
};

struct nrrw_process {
   public:
      typedef boost::adjacency_list<boost::listS, boost::vecS, boost::undirectedS, 
                                    boost::no_property, boost::no_property> graph_t;
      typedef boost::graph_traits<graph_t>::vertex_descriptor vertex_t;
      typedef boost::graph_traits<graph_t>::edge_descriptor edge_t;

      nrrw_process(const unsigned int& p) : parameter(p), counter(0) {
         current = boost::add_vertex(graph);
         boost::add_edge(current, current, graph);
      }

      unsigned int step() {
         // choose an outgoing edge randomly
         typename boost::graph_traits<graph_t>::out_edge_iterator start, end;
         boost::tie(start, end) = boost::out_edges(current, graph);

         auto random_iter = selector.select(start, end);
         const edge_t& random_edge = *random_iter;
         
         // move to the random vertex
         current = boost::target(random_edge, graph);
         counter++;

         // add a new vertex to the graph if necessary
         if (counter % parameter == 0) {
            vertex_t new_vertex = boost::add_vertex(graph);
            boost::add_edge(current, new_vertex, graph);
         }

         return counter;
      }

      std::vector<unsigned int> get_degrees() const {
         typename boost::graph_traits<graph_t>::vertex_iterator start, end;
         boost::tie(start, end) = boost::vertices(graph);

         const unsigned int size = boost::num_vertices(graph);
         std::vector<unsigned int> degrees(size);
         std::transform(start, end, degrees.begin(),
                       [this](vertex_t v){return this->degree(v);});
         return degrees;
      }

      void write_graphviz(const std::string& filename) {
         std::ofstream file(filename);
         vertex_writer v_writer;
         graph_writer g_writer;
         boost::write_graphviz(file, graph, v_writer, boost::default_writer(), g_writer);
         file.close();
      }

   private:
      graph_t graph;
      vertex_t current;

      random_selector<> selector;
      unsigned int counter = 0;
      const unsigned int& parameter;

      unsigned int degree(vertex_t vertex) const {
         return boost::out_degree(vertex, graph);
      }

      struct vertex_writer {
         template <class VertexOrEdge>
         void operator()(std::ostream& out, const VertexOrEdge& v) const {
            out << "[label=\"\"]";
         }
      };

      struct graph_writer {
         void operator()(std::ostream& out) const {
            out << "graph [ranksep=4 nodesep=1]" << std::endl;
            out << "node [shape=point]" << std::endl;
         }
      };
};

int main(int argc, char* argv[]) {
   if (argc < 3) {
      std::cerr << "Usage: " << argv[0] << " PARAMETER STEPS" << std::endl;
      return 1;
   }

   const int parameter = atoi(argv[1]);
   const int steps = atoi(argv[2]);

   nrrw_process process(parameter);

   while (process.step() < steps) {}
   auto degrees = process.get_degrees();

   std::ofstream file("degrees.csv");
   for (auto it = degrees.cbegin(); it != degrees.cend(); it++) {
      file << *it << std::endl;
   }
   file.close();

   process.write_graphviz("graph.dot");
}
