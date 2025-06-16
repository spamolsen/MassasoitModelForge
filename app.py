import dash
from dash import dcc, html, Input, Output, State, callback, dash_table
import dash_bootstrap_components as dbc
import plotly.express as px
import plotly.graph_objects as go
import pandas as pd
import numpy as np
import base64
import io
import os
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri, Formula
import rpy2.robjects as ro
from rpy2.robjects.conversion import localconverter

# Initialize R packages
base = importr('base')
stats = importr('stats')
mgcv = importr('mgcv')

# Initialize Dash app
app = dash.Dash(__name__, external_stylesheets=[dbc.themes.BOOTSTRAP])
app.title = "Massasoit Model Forge - GAM Analysis"

# App layout
app.layout = dbc.Container([
    dbc.Row([
        dbc.Col(html.H1("Massasoit Model Forge - GAM Analysis", className="text-center my-4"), width=12)
    ]),
    
    # File Upload Section
    dbc.Row([
        dbc.Col([
            dbc.Card([
                dbc.CardHeader("1. Upload Data"),
                dbc.CardBody([
                    dcc.Upload(
                        id='upload-data',
                        children=html.Div([
                            'Drag and Drop or ',
                            html.A('Select Files')
                        ]),
                        style={
                            'width': '100%',
                            'height': '60px',
                            'lineHeight': '60px',
                            'borderWidth': '1px',
                            'borderStyle': 'dashed',
                            'borderRadius': '5px',
                            'textAlign': 'center',
                            'margin': '10px 0'
                        },
                        multiple=False
                    ),
                    html.Div(id='output-data-upload'),
                ])
            ])
        ], width=12)
    ]),
    
    # Model Configuration Section
    dbc.Row([
        dbc.Col([
            dbc.Card([
                dbc.CardHeader("2. Configure GAM Model"),
                dbc.CardBody([
                    dbc.Row([
                        dbc.Col([
                            html.Label('Response Variable'),
                            dcc.Dropdown(id='response-var', options=[], placeholder="Select response variable")
                        ], width=6),
                        dbc.Col([
                            html.Label('Predictor Variables'),
                            dcc.Dropdown(id='predictor-vars', options=[], multi=True, placeholder="Select predictor variables")
                        ], width=6)
                    ]),
                    dbc.Row([
                        dbc.Col([
                            html.Label('Family'),
                            dcc.Dropdown(
                                id='family',
                                options=[
                                    {'label': 'Gaussian', 'value': 'gaussian'},
                                    {'label': 'Binomial', 'value': 'binomial'},
                                    {'label': 'Poisson', 'value': 'poisson'},
                                    {'label': 'Gamma', 'value': 'Gamma'}
                                ],
                                value='gaussian',
                                clearable=False
                            )
                        ], width=6),
                        dbc.Col([
                            html.Label('Smoothing Parameter (k)'),
                            dcc.Input(id='k-value', type='number', value=10, min=3, step=1)
                        ], width=6)
                    ], className="mt-3"),
                    dbc.Button('Run Analysis', id='run-analysis', color='primary', className='mt-3')
                ])
            ])
        ], width=12)
    ], className="mt-4"),
    
    # Results Section
    dbc.Row([
        dbc.Col([
            dbc.Card([
                dbc.CardHeader("3. Model Results"),
                dbc.CardBody([
                    html.Div(id='model-summary'),
                    dcc.Loading(
                        id="loading-results",
                        type="circle",
                        children=[
                            html.Div(id='model-results'),
                            html.Div(id='model-plots')
                        ]
                    )
                ])
            ])
        ], width=12)
    ], className="mt-4"),
    
    # Store for data
    dcc.Store(id='stored-data')
], fluid=True)

# Callbacks
@callback(
    [Output('stored-data', 'data'),
     Output('output-data-upload', 'children'),
     Output('response-var', 'options'),
     Output('predictor-vars', 'options')],
    [Input('upload-data', 'contents')],
    [State('upload-data', 'filename')]
)
def update_output(contents, filename):
    if contents is None:
        return None, "No file uploaded", [], []
    
    content_type, content_string = contents.split(',')
    decoded = base64.b64decode(content_string)
    
    try:
        if 'csv' in filename:
            df = pd.read_csv(io.StringIO(decoded.decode('utf-8')))
        elif 'xls' in filename:
            df = pd.read_excel(io.BytesIO(decoded))
        else:
            return None, "Unsupported file format. Please upload a CSV or Excel file.", [], []
            
        # Create options for dropdowns
        options = [{'label': col, 'value': col} for col in df.columns]
        
        # Store data as JSON and return
        return df.to_json(date_format='iso', orient='split'), \
               f"Successfully loaded {filename} with {df.shape[0]} rows and {df.shape[1]} columns.", \
               options, options
    except Exception as e:
        return None, f"Error loading file: {str(e)}", [], []

@callback(
    [Output('model-summary', 'children'),
     Output('model-results', 'children'),
     Output('model-plots', 'children')],
    [Input('run-analysis', 'n_clicks')],
    [State('stored-data', 'data'),
     State('response-var', 'value'),
     State('predictor-vars', 'value'),
     State('family', 'value'),
     State('k-value', 'value')]
)
def run_gam_analysis(n_clicks, json_data, response_var, predictor_vars, family, k_value):
    if n_clicks is None or json_data is None or not response_var or not predictor_vars:
        return "", "", ""
    
    try:
        # Convert JSON back to pandas DataFrame
        df = pd.read_json(io.StringIO(json_data), orient='split')
        
        # Convert to R dataframe
        with localconverter(ro.default_converter + pandas2ri.converter):
            r_df = ro.conversion.py2rpy(df)
        
        # Create formula
        formula = f"{response_var} ~ " + " + ".join([f"s({var}, k={k_value})" for var in predictor_vars])
        
        # Fit GAM model
        gam_fit = mgcv.gam(
            Formula(formula),
            data=r_df,
            family=family
        )
        
        # Get model summary
        summary = base.summary(gam_fit)
        
        # Convert R summary to string
        summary_text = []
        for line in base.capture_output(summary):
            summary_text.append(html.P(str(line)))
        
        # Create diagnostic plots
        plots = []
        for i, var in enumerate(predictor_vars):
            # Create partial effect plot for each predictor
            fig = go.Figure()
            
            # Get partial effect predictions
            new_data = {}
            for v in predictor_vars:
                if v == var:
                    # Create a sequence of values for the current variable
                    x_vals = np.linspace(df[var].min(), df[var].max(), 100)
                    new_data[v] = x_vals
                else:
                    # Use mean for other variables
                    new_data[v] = [df[v].mean()] * 100
            
            # Convert to R dataframe
            with localconverter(ro.default_converter + pandas2ri.converter):
                r_new_data = ro.conversion.py2rpy(pd.DataFrame(new_data))
            
            # Get predictions
            pred = stats.predict(gam_fit, newdata=r_new_data, type="response", se=True)
            
            # Convert predictions to numpy arrays
            y_pred = np.array(pred[0])
            y_se = np.array(pred[1])
            
            # Add trace for main effect
            fig.add_trace(go.Scatter(
                x=x_vals,
                y=y_pred,
                mode='lines',
                name='Predicted',
                line=dict(color='blue')
            ))
            
            # Add confidence interval
            fig.add_trace(go.Scatter(
                x=np.concatenate([x_vals, x_vals[::-1]]),
                y=np.concatenate([y_pred + 1.96 * y_se, (y_pred - 1.96 * y_se)[::-1]]),
                fill='toself',
                fillcolor='rgba(0,100,80,0.2)',
                line=dict(color='rgba(255,255,255,0)'),
                showlegend=False,
                name='95% CI'
            ))
            
            # Update layout
            fig.update_layout(
                title=f'Partial effect of {var}',
                xaxis_title=var,
                yaxis_title=f'f({var})',
                template='plotly_white',
                height=400
            )
            
            plots.append(dcc.Graph(figure=fig))
        
        # Create model summary card
        summary_card = dbc.Card([
            dbc.CardHeader("Model Summary"),
            dbc.CardBody([
                html.Pre(summary_text, style={'white-space': 'pre-wrap', 'font-family': 'monospace'})
            ])
        ])
        
        return summary_card, "", html.Div(plots)
    
    except Exception as e:
        error_message = f"Error running GAM analysis: {str(e)}"
        return error_message, "", ""

if __name__ == '__main__':
    app.run_server(debug=True)
from sklearn.datasets import make_classification