#!/bin/bash

# Create virtual environment
echo "Creating Python virtual environment..."
python3 -m venv venv

# Activate the virtual environment
echo "Activating virtual environment..."
source venv/bin/activate

# Upgrade pip
echo "Upgrading pip..."
python -m pip install --upgrade pip

# Install requirements
echo "Installing Python dependencies..."
pip install -r requirements.txt

echo -e "\nVirtual environment setup complete!"
echo "To activate the virtual environment in the future, run:"
echo "    source venv/bin/activate"
