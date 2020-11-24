"""
Created on Thu Dec 12 09:59:46 2019

@author: jan-philippfranken
"""

### creating the plots for illustrating bscm model

# libraries
from scipy.stats import norm
import plotly.graph_objects as go


# creating data for plotting
def create_arrays(start_val, stop_val, cardinality, mean, var):
    x_arr = []
    y_arr = []
    step = (stop_val - start_val) / (cardinality - 1)
    for i in range(cardinality):
        x = start_val + (step * i)
        y = norm.pdf(x, mean, var)
        x_arr.append(x)
        y_arr.append(y)
    return x_arr, y_arr


x_e, y_e = create_arrays(0.01, .99, 1000, 0.5, 0.25)
x_t, y_t = create_arrays(0.01, .99, 1000, 0.5, 0.25)
x_pH, y_pH = create_arrays(0.01, .99, 1000, 0.5, 0.25)


# plotting data
fig_e = go.Figure()
fig_t = go.Figure()
fig_pH = go.Figure()

# expertise plot
fig_e.add_trace(go.Scatter(
        x=x_e, y=y_e,
        name = False,
        type = 'scatter',
        fill ='tozeroy',
        mode = 'lines',
        line = {'color':'silver'}
        ))

# trustworthiness plot
fig_t.add_trace(go.Scatter(
        x=x_e, y=y_e,
        name = False,
        type = 'scatter',
        fill ='tozeroy',
        mode = 'lines',
        line = {'color':'silver'}
        ))

# p(h) plot
fig_pH.add_trace(go.Scatter(
        x=x_e[:500], y=y_e[:500],
        name = False,
        type = 'scatter',
        fill ='tozeroy',
        mode = 'lines',
        line = {'color':'blue'}
        ))

fig_pH.add_trace(go.Scatter(
        x=x_e[500:], y=y_e[500:],
        name = False,
        type = 'scatter',
        fill ='tozeroy',
        mode = 'lines',
        line = {'color':'red'}
        ))

# layout of each subplot
def fig_design(fig):
    fig.update_traces(mode='markers', marker_line_width=2, marker_size=10)
    fig.update_layout(hovermode = False,
                      template = 'none',
                      font=dict(
                          family="Courier New, monospace",
                          size=25,
                          color="#7f7f7f"),
                      title = {
                          'text': '',
                           'x':0.5},
                      yaxis = {
                          'fixedrange': True,
                          'showgrid': False,
                          'zeroline': False,
                          'showticklabels': False},
                      xaxis = {
                          'fixedrange': True,
                           'showgrid': False,
                           'zeroline': False,
                           'tickvals': [.01, .5, .99],
                           'ticktext': ['0.00', '0.50', '1.00']})

    fig.update_layout(showlegend=False)
    fig.show()

figures = [fig_e, fig_t, fig_pH]

for fig in figures:
    fig_design(fig)
