/*
	Sgl template library for optimizing sparse group lasso penalized objectives.
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

#ifndef LOGIT_RESPONSE_H_
#define LOGIT_RESPONSE_H_

using namespace sgl;

class LogitResponse : public elements < LogitResponse > {

private:
	vector const linear_predictors;

public:

	LogitResponse(vector const& linear_predictors) :
		linear_predictors(linear_predictors) {}

	//Needed so that we can use fields
	LogitResponse() :
		linear_predictors(null_vector) {}

	LogitResponse const& operator=(LogitResponse const& s)
	{
		const_cast < sgl::vector & > ( this->linear_predictors ) = s.linear_predictors;

		return * this;
	}

		rList as_rList() const {

			vector prob = exp(linear_predictors)/(1+exp(linear_predictors));

	    rList list;
	    list.attach( linear_predictors, "link");
			list.attach( prob, "prob");

	    return list;
	  }

};


#endif /* LOGIT_RESPONSE_H_ */
