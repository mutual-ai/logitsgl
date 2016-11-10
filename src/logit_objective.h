/* Routines for linear multiple output using sparse group lasso regression.
 Intended for use with R.
 Copyright (C) 2014 Martin Vincent

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>
 */

#ifndef LOGIT_OBJECTIVE_H_
#define LOGIT_OBJECTIVE_H_

//type_X : sgl::matrix or sgl::sparse_matrix
//type_Y : sgl::matrix or sgl::sparse_matrix

template < typename type_X, typename type_Y >
class LogitLoss {

public:

	const sgl::natural n_variables;

	typedef sgl::hessian_diagonal<false> hessian_type;

	typedef sgl::DataPackage_2<
		sgl::MatrixData<type_X>,
		sgl::MultiResponse<type_Y, 'Y'> > data_type
	;

private:

	type_Y const& Y; //response - 0 1 matrix of size n_samples x n_responses

	// state variables
	sgl::matrix prob; //probabilities
	mutable sgl::matrix H; //hessians
	mutable double value;

	// states
	mutable bool hessians_computed;
	mutable bool value_computed;

public:

	LogitLoss() :
		n_variables(0),
		Y(sgl::null_matrix),
		prob(sgl::null_matrix),
		H(sgl::null_matrix),
		value(0),
		hessians_computed(false),
		value_computed(false) {}

	LogitLoss(data_type const& data) :
		n_variables(data.get_B().n_responses),
		Y(data.get_B().response),
		prob(data.get_A().n_samples, n_variables),
		H(n_variables, data.get_A().n_samples),
		value(0),
		hessians_computed(false),
		value_computed(false) {}

	void set_lp(sgl::matrix const& lp) {

		TIMER_START

		prob = trunc_exp(lp);
		prob = prob/(1+prob);

		ASSERT_IS_FINITE(prob);

		hessians_computed = false;
		value_computed = false;
	}

	void set_lp_zero() {

		prob.ones();
		prob = prob/(1+prob);

		hessians_computed = false;
		value_computed = false;
	}

	const sgl::matrix gradients() const	{

		TIMER_START

		return -trans(Y - prob);
	}

	void compute_hessians() const	{

		TIMER_START

		if(hessians_computed) {
			return;
		}

		// Compute hessian
		H = trans(prob - square(prob));

		hessians_computed = true;
		return;
	}

  const sgl::vector hessians(sgl::natural i) const {

		TIMER_START

		return H.col(i);
	}

	const sgl::numeric sum_values() const	{

		TIMER_START

		if( ! value_computed) {

			value = -accu(Y%log(prob)-Y%log(1-prob)+log(1-prob));

			value_computed = true;
		}

			return value;
	}

};

typedef sgl::ObjectiveFunctionType <
	sgl::GenralizedLinearLossDense <
		LogitLoss < sgl::matrix, sgl::matrix > > > logit
;

typedef sgl::ObjectiveFunctionType <
	sgl::GenralizedLinearLossSparse <
		LogitLoss < sgl::sparse_matrix, sgl::matrix > > > logit_spx
;

typedef sgl::ObjectiveFunctionType <
	sgl::GenralizedLinearLossDense <
		LogitLoss < sgl::matrix, sgl::sparse_matrix > > > logit_spy
;

typedef sgl::ObjectiveFunctionType <
	sgl::GenralizedLinearLossSparse <
		LogitLoss < sgl::sparse_matrix, sgl::sparse_matrix > > > logit_spx_spy
;

#endif /* LOGIT_OBJECTIVE_H_ */
