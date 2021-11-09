//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0
import JASP.Theme 1.0
import "../qml/qml_components" as LS

Form 
{
	columns: 1
	
	Section
	{
	title: qsTr("Parent Distribution")
		
		DropDown
		{
		name: "cltParentDistribution"
		label: qsTr("Parent Distribution")
		indexDefaultValue: 0
		values:
		[
		{label: qsTr("Normal"),		value: "normal"},
		{label: qsTr("Uniform"),	value: "uniform"},
		{label: qsTr("Skewed"),		value: "skewed"}
		]
		id: cltParentDistribution
		}
	
		DoubleField
		{
		name: "cltMean"
		label: qsTr("Mean")
		fieldWidth: 60
		defaultValue: 0
		decimals: 2
		}
	
		DoubleField
		{
		name: "cltStdDev"
		label: qsTr("Std. Dev.")
		fieldWidth: 60
		defaultValue: 1
		decimals: 2
		min:			0.01
		}
	
		DropDown
		{
		name: "cltSkewDirection"
		label: qsTr("Skew Direction")
		indexDefaultValue: 0
		visible: cltParentDistribution.currentValue == "skewed"
		values:
		[
		{label: qsTr("Left"),		value: "left"},
		{label: qsTr("Right"),		value: "right"}
		]
		id: cltSkewDirection
		}
	
		DropDown
		{
		name: "cltSkewIntensity"
		label: qsTr("Skew Intensity")
		indexDefaultValue: 0
		visible: cltParentDistribution.currentValue == "skewed"
		values:
		[
		{label: qsTr("Low skew"),		value: "low"},
		{label: qsTr("Medium skew"),		value: "medium"},
		{label: qsTr("High skew"),		value: "high"}
		]
		id: cltSkewIntensity
		}
	}
		Section
	{
	title: qsTr("Sample Options")
	
		DoubleField
		{
		name: "cltSampleSize"
		label: qsTr("Sample Size")
		fieldWidth: 60
		defaultValue: 30
		decimals: 0
		}
		
		DoubleField
		{
		name: "cltSampleAmount"
		label: qsTr("Number of Samples")
		fieldWidth: 60
		defaultValue: 100
		decimals: 0
		}
	}
	
		Section
	{
	title: qsTr("Sampling Distribution Options")
	
	CheckBox{name: "SamplingDistShowNormal";		label: qsTr("Superimpose Normal Distribution"); checked: true}
	
	}
}
