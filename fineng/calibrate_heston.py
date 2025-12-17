#!/usr/bin/python3

import sys
import os

ql_path = "/home/anuragr/.local/lib/python3.13/site-packages/QuantLib/QuantLib.py"
sys.path.insert(0, ql_path)

import QuantLib as ql

print("QuantLib version:", ql.__version__)
import QuantLib as ql
import numpy as np
import matplotlib.pyplot as plt
from datetime import datetime, timedelta

def setup_quantlib_environment():
    """Set up the basic QuantLib environment"""
    # Set the evaluation date
    today = datetime(2024, 1, 15)
    ql.Settings.instance().evaluationDate = ql.Date(today.day, today.month, today.year)
    
    # Market data
    spot_price = 100.0
    risk_free_rate = 0.05  # 5%
    dividend_yield = 0.02  # 2%
    day_count = ql.Actual365Fixed()
    
    # Fixed: Specify the market for UnitedStates calendar
    calendar = ql.UnitedStates(ql.UnitedStates.NYSE)
    
    return spot_price, risk_free_rate, dividend_yield, day_count, calendar

def create_market_data(spot_price, risk_free_rate, dividend_yield, day_count, calendar):
    """Create market data term structures"""
    # Flat term structures for simplicity
    flat_risk_free = ql.FlatForward(0, calendar, risk_free_rate, day_count)
    flat_dividend = ql.FlatForward(0, calendar, dividend_yield, day_count)
    flat_vol = ql.BlackConstantVol(0, calendar, 0.20, day_count)  # Initial guess
    
    # Create handles
    risk_free_handle = ql.YieldTermStructureHandle(flat_risk_free)
    dividend_handle = ql.YieldTermStructureHandle(flat_dividend)
    vol_handle = ql.BlackVolTermStructureHandle(flat_vol)
    spot_handle = ql.QuoteHandle(ql.SimpleQuote(spot_price))
    
    return spot_handle, risk_free_handle, dividend_handle, vol_handle

def create_calibration_helpers(spot_handle, risk_free_handle, dividend_handle, calendar, day_count):
    """Create option data and calibration helpers"""
    
    # More realistic market option data with consistent pricing
    market_data = [
        # Near-term options (1 month)
        (1, 95, 6.82, ql.Option.Call), (1, 100, 3.52, ql.Option.Call), (1, 105, 1.45, ql.Option.Call),
        (1, 95, 2.15, ql.Option.Put), (1, 100, 3.85, ql.Option.Put), (1, 105, 6.65, ql.Option.Put),
        
        # Medium-term options (3 months)
        (3, 95, 8.45, ql.Option.Call), (3, 100, 5.62, ql.Option.Call), (3, 105, 3.58, ql.Option.Call),
        (3, 95, 3.72, ql.Option.Put), (3, 100, 5.92, ql.Option.Put), (3, 105, 8.85, ql.Option.Put),
        
        # Longer-term options (6 months)
        (6, 95, 10.85, ql.Option.Call), (6, 100, 8.25, ql.Option.Call), (6, 105, 6.15, ql.Option.Call),
        (6, 95, 5.95, ql.Option.Put), (6, 100, 8.25, ql.Option.Put), (6, 105, 11.25, ql.Option.Put),
    ]
    
    helpers = []
    option_data = []
    
    for maturity_months, strike, market_price, option_type in market_data:
        # Calculate maturity date
        maturity_date = calendar.advance(ql.Settings.instance().evaluationDate, 
                                       ql.Period(maturity_months, ql.Months))
        
        # Create payoff and exercise
        payoff = ql.PlainVanillaPayoff(option_type, strike)
        exercise = ql.EuropeanExercise(maturity_date)
        
        # Create the option
        option = ql.EuropeanOption(payoff, exercise)
        
        # Create calibration helper
        helper = ql.HestonModelHelper(
            ql.Period(maturity_months, ql.Months), calendar, 
            spot_handle.value(), strike, 
            ql.QuoteHandle(ql.SimpleQuote(market_price)),
            risk_free_handle, dividend_handle)
        
        helpers.append(helper)
        option_data.append({
            'maturity': maturity_months,
            'strike': strike,
            'market_price': market_price,
            'type': 'Call' if option_type == ql.Option.Call else 'Put',
            'helper': helper
        })
    
    return helpers, option_data

def numpy_to_ql_matrix(np_array):
    """Convert numpy array to QuantLib Matrix"""
    rows, cols = np_array.shape
    ql_matrix = ql.Matrix(rows, cols)
    for i in range(rows):
        for j in range(cols):
            ql_matrix[i][j] = np_array[i, j]
    return ql_matrix

def calculate_implied_volatilities(option_data, spot_handle, risk_free_handle, dividend_handle, calendar, day_count):
    """Calculate implied volatilities from market prices"""
    print("\nCalculating implied volatilities...")
    
    for data in option_data:
        strike = data['strike']
        market_price = data['market_price']
        option_type = ql.Option.Call if data['type'] == 'Call' else ql.Option.Put
        maturity_date = calendar.advance(ql.Settings.instance().evaluationDate, 
                                       ql.Period(data['maturity'], ql.Months))
        
        # Create option
        payoff = ql.PlainVanillaPayoff(option_type, strike)
        exercise = ql.EuropeanExercise(maturity_date)
        option = ql.EuropeanOption(payoff, exercise)
        
        # Create Black-Scholes process with initial vol guess
        flat_vol = ql.BlackConstantVol(0, calendar, 0.20, day_count)
        process = ql.BlackScholesMertonProcess(spot_handle, dividend_handle, 
                                             risk_free_handle, 
                                             ql.BlackVolTermStructureHandle(flat_vol))
        
        # Calculate implied volatility
        try:
            implied_vol = option.impliedVolatility(market_price, process, 1e-6, 1000, 1e-8, 4.0)
            data['implied_vol'] = implied_vol
            print(f"{data['type']} K={strike}, T={data['maturity']}M: IV = {implied_vol:.4f}")
        except:
            data['implied_vol'] = 0.20  # Default fallback
            print(f"{data['type']} K={strike}, T={data['maturity']}M: Using default IV = 0.20")
    
    return option_data

def create_simple_volatility_surface(option_data, calendar, day_count):
    """Create a simple volatility surface"""
    print("\nCreating simple volatility surface...")
    
    # Extract unique maturities and strikes
    maturities = sorted(set(data['maturity'] for data in option_data))
    strikes = sorted(set(data['strike'] for data in option_data))
    
    print(f"Maturities: {maturities}")
    print(f"Strikes: {strikes}")
    
    # Create volatility matrix as numpy array first
    vol_matrix_np = np.zeros((len(maturities), len(strikes)))
    
    # Fill the matrix with actual implied vols
    for i, maturity in enumerate(maturities):
        for j, strike in enumerate(strikes):
            # Find matching option data
            matching_data = [data for data in option_data 
                           if data['maturity'] == maturity and data['strike'] == strike 
                           and 'implied_vol' in data]
            if matching_data:
                vol_matrix_np[i, j] = matching_data[0]['implied_vol']
                print(f"T={maturity}M, K={strike}: IV = {matching_data[0]['implied_vol']:.4f}")
            else:
                # If no exact match, use average for this maturity
                maturity_vols = [data['implied_vol'] for data in option_data 
                               if data['maturity'] == maturity and 'implied_vol' in data]
                avg_vol = np.mean(maturity_vols) if maturity_vols else 0.20
                vol_matrix_np[i, j] = avg_vol
                print(f"T={maturity}M, K={strike}: Using average IV = {avg_vol:.4f}")
    
    # Convert numpy array to QuantLib Matrix - FIXED
    vol_matrix_ql = numpy_to_ql_matrix(vol_matrix_np)
    
    # Create dates for maturities
    dates = []
    for maturity in maturities:
        date = calendar.advance(ql.Settings.instance().evaluationDate, 
                              ql.Period(maturity, ql.Months))
        dates.append(date)
    
    print(f"Dates: {[d for d in dates]}")
    
    # Create Black volatility surface - FIXED: using QuantLib Matrix
    vol_surface = ql.BlackVarianceSurface(
        ql.Settings.instance().evaluationDate,
        calendar,
        dates,
        strikes,
        vol_matrix_ql,  # Now using QuantLib Matrix instead of numpy array
        day_count
    )
    
    # Enable extrapolation
    vol_surface.enableExtrapolation()
    
    return vol_surface, maturities, strikes, vol_matrix_np

def calibrate_heston_model(helpers, spot_handle, risk_free_handle, dividend_handle):
    """Calibrate Heston model to market data"""
    print("Attempting Heston model calibration...")
    
    # Conservative initial parameters
    initial_parameters = [0.04, 1.5, 0.04, 0.25, -0.3]
    
    # Create Heston process
    process = ql.HestonProcess(risk_free_handle, dividend_handle, spot_handle, 
                             initial_parameters[0], initial_parameters[1],
                             initial_parameters[2], initial_parameters[3],
                             initial_parameters[4])
    
    model = ql.HestonModel(process)
    engine = ql.AnalyticHestonEngine(model)
    
    for helper in helpers:
        helper.setPricingEngine(engine)
    
    # Calibration settings
    optimization_method = ql.LevenbergMarquardt(1e-5, 1e-5, 1e-5)
    end_criteria = ql.EndCriteria(500, 100, 1e-6, 1e-6, 1e-6)
    
    try:
        model.calibrate(helpers, optimization_method, end_criteria)
        calibrated_params = model.params()
        
        print("\nHeston calibration completed!")
        print(f"v0: {calibrated_params[0]:.6f}")
        print(f"kappa: {calibrated_params[1]:.6f}")
        print(f"theta: {calibrated_params[2]:.6f}")
        print(f"sigma: {calibrated_params[3]:.6f}")
        print(f"rho: {calibrated_params[4]:.6f}")
        
        return model, calibrated_params
    except Exception as e:
        print(f"Heston calibration failed: {e}")
        return None, None

def plot_volatility_surface(maturities, strikes, vol_matrix, calibrated_params=None):
    """Plot the volatility surface"""
    try:
        fig = plt.figure(figsize=(12, 5))
        
        # Create meshgrid for 3D plot
        M, K = np.meshgrid(maturities, strikes, indexing='ij')
        
        # Plot 3D surface
        ax1 = fig.add_subplot(121, projection='3d')
        surf = ax1.plot_surface(M, K, vol_matrix * 100, cmap='viridis', alpha=0.8)
        ax1.set_xlabel('Maturity (Months)')
        ax1.set_ylabel('Strike Price')
        ax1.set_zlabel('Implied Volatility (%)')
        ax1.set_title('Implied Volatility Surface')
        
        # Plot 2D smile for each maturity
        ax2 = fig.add_subplot(122)
        for i, maturity in enumerate(maturities):
            ax2.plot(strikes, vol_matrix[i] * 100, 'o-', label=f'{maturity}M', markersize=4)
        
        ax2.set_xlabel('Strike Price')
        ax2.set_ylabel('Implied Volatility (%)')
        ax2.set_title('Volatility Smile by Maturity')
        ax2.legend()
        ax2.grid(True, alpha=0.3)
        
        if calibrated_params:
            ax2.text(0.05, 0.95, f"Heston Parameters:\n"
                    f"v₀: {calibrated_params[0]:.4f}\n"
                    f"κ: {calibrated_params[1]:.4f}\n"
                    f"θ: {calibrated_params[2]:.4f}\n"
                    f"σ: {calibrated_params[3]:.4f}\n"
                    f"ρ: {calibrated_params[4]:.4f}",
                    transform=ax2.transAxes, verticalalignment='top',
                    bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
        
        plt.tight_layout()
        plt.show()
    except Exception as e:
        print(f"Plotting failed: {e}")

def main():
    """Main calibration routine"""
    print("QuantLib Volatility Curve Calibration")
    print("=" * 50)
    
    try:
        # Step 1: Set up environment
        spot_price, risk_free_rate, dividend_yield, day_count, calendar = setup_quantlib_environment()
        print(f"Evaluation date: {ql.Settings.instance().evaluationDate}")
        print(f"Spot price: {spot_price}")
        
        # Step 2: Create market data
        spot_handle, risk_free_handle, dividend_handle, vol_handle = create_market_data(
            spot_price, risk_free_rate, dividend_yield, day_count, calendar)
        
        # Step 3: Create calibration helpers
        helpers, option_data = create_calibration_helpers(
            spot_handle, risk_free_handle, dividend_handle, calendar, day_count)
        print(f"Created {len(helpers)} calibration helpers")
        
        # Step 4: Calculate implied volatilities
        option_data = calculate_implied_volatilities(
            option_data, spot_handle, risk_free_handle, dividend_handle, calendar, day_count)
        
        # Step 5: Try Heston calibration
        heston_model, calibrated_params = calibrate_heston_model(
            helpers, spot_handle, risk_free_handle, dividend_handle)
        
        # Step 6: Create volatility surface
        vol_surface, maturities, strikes, vol_matrix = create_simple_volatility_surface(
            option_data, calendar, day_count)
        
        # Step 7: Plot results
        plot_volatility_surface(maturities, strikes, vol_matrix, calibrated_params)
        
        # Step 8: Demonstrate usage
        print("\n" + "="*50)
        print("Volatility Surface Usage Examples:")
        print("="*50)
        
        # Test the volatility surface
        test_points = [
            (2, 98),   # 2 months, strike 98
            (4, 102),  # 4 months, strike 102
            (5, 100),  # 5 months, strike 100 (interpolated)
        ]
        
        for test_maturity, test_strike in test_points:
            test_date = calendar.advance(ql.Settings.instance().evaluationDate, 
                                       ql.Period(test_maturity, ql.Months))
            try:
                vol = vol_surface.blackVol(test_date, test_strike)
                print(f"T={test_maturity}M, K={test_strike}: Vol = {vol:.4f} ({vol*100:.2f}%)")
            except Exception as e:
                print(f"T={test_maturity}M, K={test_strike}: Error - {e}")
        
        return vol_surface, heston_model, calibrated_params, option_data
        
    except Exception as e:
        print(f"Error during calibration: {e}")
        import traceback
        traceback.print_exc()
        return None, None, None, None

if __name__ == "__main__":
    vol_surface, heston_model, calibrated_params, option_data = main()

