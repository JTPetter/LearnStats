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
	
	
				DropDown
		{
		name: "LSdescCentralOrSpread"
		label: qsTr("Statistics to explain")
		indexDefaultValue: 0
		values:
		[
		{label: qsTr("Central tendency"),		value: "LSdescCentralTendency"},
		{label: qsTr("Spread"),	value: "LSdescSpread"}
		]
		id: lsDescCentralOrSpread
		}
		
	
	Section
	{
	title: qsTr("Data options")
	columns:	1
			
	RadioButtonGroup
	{
		columns:	3
		name:		"lstDescDataType"
		title:		qsTr("Data Input Type")
		id:			lstDescDataType
		
		
		
						RadioButton
		{
			value:		"dataRandom"
			label:		qsTr("Random Sample")
			id:			dataTypeA
			checked:	true
		}
		
				RadioButton
		{
			value:		"dataSequence"
			label:		qsTr("Enter sequence")
			id:			dataTypeB
		}

		RadioButton
		{
			value:		"dataVariable"
			label:		qsTr("Select variable")
			id:			dataTypeC
			enabled:	mainWindow.dataAvailable
		}



	}
	
	Group
	{
	columns: 2
	
	DoubleField
	{
				name:			"lstDescSampleN"
				visible:	dataTypeA.checked
				label:			qsTr("Sample size")
				fieldWidth:		60
				defaultValue:	100
				decimals:		0
	}
	
	DoubleField
	{
				name:			"lstDescSampleSeed"
				visible:	dataTypeA.checked
				label:			qsTr("Set seed")
				fieldWidth:		60
				defaultValue:	123
				decimals:		0
	}
	
	}
	
	
	
	Group
	{
	
		RadioButtonGroup
	{
		columns:	3
		name:		"lstDescSampleDistType"
		visible:	dataTypeA.checked
		title:		qsTr("Distribution Type")
		id:			distributionType
		
		RadioButton
		{
			value:		"lstSampleDistDiscrete"
			label:		qsTr("Discrete")
			id:			distTypeDisc
			checked:	true
		}
				
		RadioButton
		{
			value:		"lstSampleDistCont"
			label:		qsTr("Continuous")
			id:			distTypeCont
		}
		
		
	}
		
			DropDown
		{
		name: "LSdescDiscreteDistributions"
		visible:	dataTypeA.checked
		label: qsTr("Distribution")
		indexDefaultValue: 0
		values:
		[
		{label: qsTr("Various discrete distributions"),		value: "variousDiscreteDistributions"},
		]
		id: lsDescDiscreteDistributions
		}
	}


	TextArea
	{
		title:		qsTr("Comma-separated Sequence of Observations")
		visible:	dataTypeB.checked
		height:		100
		name:     "lstDescDataSequenceInput"
		textType:	JASP.TextTypeSource
		separators:	[",",";","\n"]
	}


	Group
	{
		visible: dataTypeC.checked

		VariablesForm
		{
			preferredHeight:	150

			AvailableVariablesList
			{
				name:	"allVariables"
				title:	qsTr("Available")
			}

			AssignedVariablesList
			{
				name:				"selectedVariable"
				title:				qsTr("Selected")
				singleVariable:		true
				allowedColumns:		["ordinal", "scale"]

			}
		}
	}

	}
	
	
		Section
	{
	title: qsTr("Central Tendency Measures")
	visible: lsDescCentralOrSpread.currentValue == "LSdescCentralTendency"
	

		RadioButtonGroup
		{
		title:                                  qsTr("Select Central Tendency Measure")
		name:                                   "LSdescCT"

			RadioButton
			{
			name:                               "LSdescMean"
			label:                              qsTr("Show Mean")
			checked:                            true
			}

			RadioButton
			{
			name:                               "LSdescMedian"
			label:                              qsTr("Show Median")
			}
		
			RadioButton
			{
			name:                               "LSdescMode"
			label:                              qsTr("Show Mode")
			}
			
						RadioButton
			{
			name:                               "LSdescMMM"
			label:                              qsTr("Compare All")
			}
		}
		
			CheckBox{name: "LSdescExplanationC";
			label: qsTr("Show explanation");
			checked: true
					}
					
	}
	
	
	
			Section
	{
	title: qsTr("Measures of spread")
	visible: lsDescCentralOrSpread.currentValue == "LSdescSpread"
	

		RadioButtonGroup
		{
		title:                                  qsTr("Select measure of spread")
		name:                                   "LSdescSpread"

			RadioButton
			{
			name:                               "LSdescRange"
			label:                              qsTr("Show range")
			checked:                            true
			}

			RadioButton
			{
			name:                               "LSdescQR"
			label:                              qsTr("Show quartiles")
			}
		
			RadioButton
			{
			name:                               "LSdescVar"
			label:                              qsTr("Show variance")
			}
			
			RadioButton
			{
			name:                               "LSdescSD"
			label:                              qsTr("Show standard deviation")
			}
		}
		
			CheckBox{name: "LSdescExplanationS";
			label: qsTr("Show explanation");
			checked: true
					}
					
	}
	
	
	
			Section
	{
	title: qsTr("Plots")
	
	
			CheckBox{name: "LSdescBarplot";
			label: qsTr("Barplot");
			checked: true
					}
					
			CheckBox{name: "LSdescDotPlot";
			label: qsTr("Dot plot");
			checked: true
					}
	}
	
	

}