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

	typedef sgl::DataPackage_2< sgl::MatrixData<type_X>,
				sgl::MultiResponse<type_Y, 'Y'> > data_type;


private:

	type_Y const& Y; //response - 0 1 matrix of size n_samples x n_responses
	sgl::matrix prob; //probabilities

public:

	LogitLoss() :
				n_variables(0),
				Y(sgl::null_matrix),
				prob(sgl::null_matrix) {}

	LogitLoss(data_type const& data) :
				n_variables(data.get_B().n_responses),
				Y(data.get_B().response),
				prob(data.get_A().n_samples, n_variables) {
	}

	void set_lp(sgl::matrix const& lp)
	{
		prob = exp(lp);
		prob = prob/(1+prob);

		ASSERT_IS_FINITE(prob);
	}

	void set_lp_zero()
	{
		prob.ones();
		prob = prob/(1+prob);
	}

	const sgl::matrix gradients() const
	{
		return -trans(Y - prob);
	}

	void compute_hessians() const
	{
		return;
	}

    const sgl::vector hessians(sgl::natural i) const
	{
		return -trans(square(prob.row(i))-prob.row(i));
	}

	const sgl::numeric sum_values() const
	{
		return -accu(Y%log(prob)-Y%log(1-prob)+log(1-prob));
	}

};

typedef sgl::ObjectiveFunctionType < sgl::GenralizedLinearLossDense < LogitLoss < sgl::matrix, sgl::matrix > > ,
		LogitLoss < sgl::matrix, sgl::matrix >::data_type > logit;

typedef sgl::ObjectiveFunctionType <
		sgl::GenralizedLinearLossSparse < LogitLoss < sgl::sparse_matrix, sgl::matrix > > ,
		LogitLoss < sgl::sparse_matrix, sgl::matrix >::data_type > logit_spx;

typedef sgl::ObjectiveFunctionType <
		sgl::GenralizedLinearLossDense < LogitLoss < sgl::matrix, sgl::sparse_matrix > > ,
		LogitLoss < sgl::matrix, sgl::sparse_matrix >::data_type > logit_spy;

typedef sgl::ObjectiveFunctionType <
		sgl::GenralizedLinearLossSparse < LogitLoss < sgl::sparse_matrix, sgl::sparse_matrix > > ,
		LogitLoss < sgl::sparse_matrix, sgl::sparse_matrix >::data_type > logit_spx_spy;

#endif /* LOGIT_OBJECTIVE_H_ */
