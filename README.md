# Massasoit Model Forge

A web application for running Generalized Additive Models (GAM) using R's `mgcv` package through a Python Dash interface.

## Features

- Upload your dataset (CSV or Excel)
- Select response and predictor variables
- Configure GAM model parameters
- View model summary and diagnostic plots
- Interactive visualization of partial effects

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/MassasoitModelForge.git
   ```

2. Install Python dependencies:
   ```bash
   pip install -r requirements.txt
   ```

3. Install R and required packages:
   - Download and install R from [CRAN](https://cran.r-project.org/)
   - Open R and install the required package:
     ```R
     install.packages("mgcv")
     ```

## Usage

1. Run the application:
   ```bash
   python app.py
   ```

2. Open your web browser and navigate to `http://127.0.0.1:8050/`

3. Follow the on-screen instructions to upload your data and run the GAM analysis.

## Project Structure

- `app.py` - Main application file containing the Dash app
- `requirements.txt` - Python dependencies
- `.gitignore` - Specifies intentionally untracked files to ignore
- `README.md` - Project documentation
- `assets/` - Static files (CSS, images, etc.)
  - `style.css` - Custom styles

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Built with [Dash](https://dash.plotly.com/) and [Plotly](https://plotly.com/)
- Uses [rpy2](https://rpy2.github.io/) for R-Python integration
- Styled with [Dash Bootstrap Components](https://dash-bootstrap-components.opensource.faculty.ai/)
