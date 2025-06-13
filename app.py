import dash
from dash import dcc, html, Input, Output, callback
import dash_bootstrap_components as dbc
import plotly.express as px
import pandas as pd
import numpy as np
from sklearn.datasets import make_classification

# Initialize the Dash app
app = dash.Dash(
    __name__,
    external_stylesheets=[dbc.themes.BOOTSTRAP],
    meta_tags=[{"name": "viewport", "content": "width=device-width, initial-scale=1"}]
)

# Generate sample data
def generate_sample_data():
    X, y = make_classification(
        n_samples=1000,
        n_features=20,
        n_informative=2,
        n_redundant=10,
        n_classes=2,
        random_state=42
    )
    df = pd.DataFrame(X, columns=[f"Feature_{i}" for i in range(X.shape[1])])
    df['Target'] = y
    return df

# App layout
app.layout = dbc.Container([
    dbc.Row([
        dbc.Col(html.H1("Massasoit Model Forge", className="text-center my-4"), width=12)
    ]),
    
    dbc.Row([
        dbc.Col([
            dbc.Card([
                dbc.CardHeader("Model Controls"),
                dbc.CardBody([
                    html.Label("Select Features"),
                    dcc.Dropdown(
                        id='feature-dropdown',
                        options=[
                            {'label': f'Feature {i}', 'value': f'Feature_{i}'} 
                            for i in range(20)
                        ],
                        value=['Feature_0', 'Feature_1'],
                        multi=True
                    ),
                    html.Hr(),
                    html.Label("Number of Samples"),
                    dcc.Slider(
                        id='sample-slider',
                        min=100,
                        max=1000,
                        step=100,
                        value=500,
                        marks={i: str(i) for i in range(100, 1001, 100)}
                    )
                ])
            ])
        ], md=4),
        
        dbc.Col([
            dbc.Card([
                dbc.CardHeader("Data Visualization"),
                dbc.CardBody([
                    dcc.Graph(id='scatter-plot')
                ])
            ])
        ], md=8)
    ]),
    
    dbc.Row([
        dbc.Col([
            dbc.Card([
                dbc.CardHeader("Model Performance"),
                dbc.CardBody([
                    dcc.Graph(id='performance-plot')
                ])
            ])
        ], width=12)
    ], className="mt-4")
], fluid=True)

# Callbacks
@app.callback(
    Output('scatter-plot', 'figure'),
    [Input('feature-dropdown', 'value'),
     Input('sample-slider', 'value')]
)
def update_scatter_plot(features, n_samples):
    df = generate_sample_data().sample(n=n_samples)
    
    if len(features) < 2:
        # If less than 2 features are selected, show a message
        fig = px.scatter(title="Please select at least 2 features")
    else:
        fig = px.scatter(
            df, 
            x=features[0], 
            y=features[1], 
            color='Target',
            title=f"{features[0]} vs {features[1]}"
        )
    
    return fig

@app.callback(
    Output('performance-plot', 'figure'),
    [Input('feature-dropdown', 'value')]
)
def update_performance_plot(features):
    # Simulate model performance metrics
    n_models = 5
    models = [f"Model {i+1}" for i in range(n_models)]
    accuracy = np.random.uniform(0.7, 0.95, n_models)
    
    fig = px.bar(
        x=models,
        y=accuracy,
        title="Model Performance Comparison",
        labels={'x': 'Model', 'y': 'Accuracy'},
        color=accuracy,
        color_continuous_scale='Viridis'
    )
    
    fig.update_layout(
        yaxis_range=[0, 1],
        coloraxis_showscale=False
    )
    
    return fig

# Run the app
if __name__ == '__main__':
    app.run_server(debug=True)
