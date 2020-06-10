#include <cassert>
#include <cstdlib>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <ilcplex/ilocplex.h>

ILOSTLBEGIN

// Lower bound for the length
int lower_bound_length(const vector<pair<int,int>> boxes) {
  int l = 0;
  for (int i = 0; i < boxes.size(); i++) {
    l = max(l, std::min(boxes[i].first, boxes[i].second));
  }
  return l;
}

// Upper bound for the length
int upper_bound_length(int w, int m, int width, int height) {
  int boxes_row = w / min(width,height);
  int min_rows = std::ceil(m / ((double)boxes_row));
  return min_rows * std::max(width,height);
}

bool cmp(const pair<int,int>& b1, const pair<int,int>& b2) {
  return (b1.first * b1.second) > (b2.first * b2.second);
}

bool are_equal(const pair<int,int>& b1, const pair<int,int>& b2) {
  return b1.first == b2.first && b1.second == b2.second;
}

int main(int argc, char* argv[]) {

  IloEnv env;

  try {
    if (argc != 1) return -1;
    IloInt w, n;
    IloInt l = 0;
    cin >> w >> n;
    cout << w << " " << n << endl;
    vector<pair<int,int>> boxes(n);
    int k = 0;
    while (k < n) {
      // Parse boxes
      int m, width, height;
      cin >> m >> width >> height;
      cout << m << "   " << width << " " << height << endl;
      for (int i = 0; i < m; ++i) {
        boxes[k+i] = pair<int,int>(width,height);
      }

      // Upper bounds of the length:
      //
      //     Compute a better maxLength by placing boxes with the same dimensions
      //       one next to the other until filling the width of the paper.
      //
      //     This gives better upper bounds than placing the boxes
      //       on above the other.
      //
      l += min( upper_bound_length(w, m, width, height)
              , upper_bound_length(w, m, height, width)
              );

      // Increase the number of boxes
      k += m;
    }

    // Sort boxes in decreasing order.
    sort(boxes.begin(), boxes.end(), cmp);

    IloModel model(env);

    IloNumVarArray x_tl  = IloNumVarArray(env, boxes.size(), 0, w-1);
    IloNumVarArray y_tl  = IloNumVarArray(env, boxes.size(), 0, l-1);
    // nb. bottom is no longer required.

    // Symmetry of the first box.
    x_tl[0].setBounds(0,0);
    y_tl[0].setBounds(0,0);

    IloNumVarArray width  = IloNumVarArray(env, boxes.size(), 0, w);
    IloNumVarArray height = IloNumVarArray(env, boxes.size(), 0, l-1);

    IloNumVar length(env, lower_bound_length(boxes)-1, l-1);

    IloInt i;
    IloInt j;

    for(i = 0; i < boxes.size(); i++) {
      IloInt b_width  = boxes[i].first;
      IloInt b_height = boxes[i].second;

      // Length
      model.add(length >= y_tl[i] + height[i] - 1);

      // Bounds
      model.add(x_tl[i] <= w - width[i]);

      // Rotations
      if (b_width == b_height) {
        width[i].setBounds(b_width, b_width);
        height[i].setBounds(b_height, b_height);
      }
      else {
        model.add(width[i] == b_width || width[i] == b_height);
        model.add(height[i] == b_width || height[i] == b_height);
        model.add(width[i] != height[i]);
      }

      // Overlapping
      for (j = i+1; j < boxes.size(); j++) {
        // As we
        if (are_equal(boxes[i], boxes[j])) {
          // Breaking symmetries of identical boxes by imposng ordering.
          model.add(y_tl[i] <= y_tl[j]); // strict inequality here would be incorrect.

          // overlapping can be optimized taking into account previous constraint
          model.add(  (x_tl[i]+width[i]  <= x_tl[j])
                   || (y_tl[i]+height[i] <= y_tl[j]));
        }
        else {
          model.add(  (x_tl[i]+width[i]  <= x_tl[j])
                   || (x_tl[i]           >= x_tl[j]+width[j])
                   || (y_tl[i]+height[i] <= y_tl[j])
                   || (y_tl[i]           >= y_tl[j]+height[j]));
        }
      }
    }

    // Objective Function
    model.add(IloMinimize(env, length));

    IloCplex cplex(model);
    cplex.setOut(env.getNullStream());

    if (cplex.solve()) {
       cout << (cplex.getObjValue() + 1) << endl;
       for (i = 0; i < boxes.size(); ++i) {
         IloNum x_i = cplex.getValue(x_tl[i]);
         IloNum y_i = cplex.getValue(y_tl[i]);
         IloNum w_i = cplex.getValue(width[i]);
         IloNum h_i = cplex.getValue(height[i]);
         cout << IloRound(x_i) << " " << IloRound(y_i)
              << "   "
              << IloRound(x_i+w_i-1) << " " << IloRound(y_i+h_i-1)
              << endl;
       }
    }
    else {
       cout << " No solution found" << endl;
    }
  }
  catch (IloException& ex) {
     cerr << "Error: " << ex << endl;
  }
  catch (...) {
     cerr << "Error" << endl;
  }

  env.end();
  return 0;
}
