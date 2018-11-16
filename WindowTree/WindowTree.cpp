#include "stdafx.h"

// WindowTree.cpp : Defines the entry point for the console application.
//
#include <AtlBase.h> // Conversion routines (CW2A)
#include <Windows.h> // Windows stuff
#include <map>
#include <unordered_map>
#include <assert.h>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <regex>
#include <type_traits>

// Characters to use as a filed seperator.
// Used define so can use c-string concatonation.
#define FIELD_SEPERATOR L"!"

bool output_handle_count = false;

// In case I wan to use a different map, like std::unordered_map.
// NOTE: Seems that std::unordered_map is slower than std::map in this use case.
template <typename...Ts>
using my_map = std::map<Ts...>;

struct node_t;

using hwnd_to_children_ptr_t = my_map<HWND, node_t*>;
using hwnd_to_allocated_childern_t = my_map<HWND, node_t>;

enum window_type_e {
	invalid,        /// HWND specified by user, but is invalid.
	special,        /// Special window (desktop or root).  Can't be reached by window enumeration.
	message,        /// Message window.
	top_level,      /// Top level window.
	not_top_level,  /// Not a top level window.
	user_specified, /// HWND specified by user and was not found before.
	not_enumable,   /// Other window that can't be reached by window enumeration.
	brute_force,    /// Specified by using brute force method (iterating over each possible HWND value).
};

std::wostream& operator<<(std::wostream& os, window_type_e status) {
	switch (status)
	{
	case invalid:
		os << "       invalid";
		break;
	case special:
		os << "       special";
		break;
	case message:
		os << "       message";
		break;
	case top_level:
		os << "     top level";
		break;
	case not_top_level:
		os << " not top level";
		break;
	case user_specified:
		os << "user specified";
		break;
	case not_enumable:
		os << "  not enumable";
		break;
	case brute_force:
		os << "   brute force";
		break;
	default:
		assert(false);
		break;
	}
	return os;
}

enum verify_correctly_parented_e {
	correct,               /// Correctly parented
	parent_node_not_found, /// Parent node not found
	parent_was_null,       /// Parent node was null and not expected
	parent_was_different   /// Parent node was different than expected
}; 

std::wostream& operator<< (std::wostream& os, verify_correctly_parented_e value)
{
	switch (value)
	{
	case correct:
		os << "correct";
		break;
	case parent_node_not_found:
		os << "parent node not found";
		break;
	case parent_was_null:
		os << "parent node was null";
		break;
	case parent_was_different:
		os << "parent node was wrong";
		break;
	default:
		assert(false);
		break;
	}
	return os;
}

void output_heading(std::wostream& os)
{
	if (output_handle_count) {
		os << "handle #" FIELD_SEPERATOR;
	}
	os << "window_status"
		FIELD_SEPERATOR "got expected"
		FIELD_SEPERATOR "visible"
		FIELD_SEPERATOR "tree"
//		FIELD_SEPERATOR "pad"
		FIELD_SEPERATOR "class name"
		FIELD_SEPERATOR "window title\n";
}

void output_window_class_and_title(std::wostream & os, const HWND &hWnd)
{
	wchar_t window_class[1024], window_title[1024];
	window_class[0] = window_title[0] = 0;

	::GetClassNameW(hWnd, window_class, _countof(window_class));
	::GetWindowTextW(hWnd, window_title, _countof(window_title));
	// replacing any CRLFs with field separators
	auto wc
		= std::regex_replace(window_class, std::wregex(L"(\r\n?|\n\r?)")
			, FIELD_SEPERATOR );
	auto wt
		= std::regex_replace(window_title, std::wregex(L"(\r\n?|\n\r?)")
			, FIELD_SEPERATOR );

	os << CW2A(wc.c_str()) << FIELD_SEPERATOR << CW2A(wt.c_str()) << std::endl;
}

struct node_t;

/// Node allocation done here.  They are referenced by HWND.
hwnd_to_allocated_childern_t all_nodes;

/// Node to handle information about it.
struct node_t
{
	/// Type of window
	window_type_e m_eType;

	/// HWND that this window is referencing
	HWND m_hCurrent;

	/// Parent node where m_pParent->m_hCurrent == ::GetParent(m_hCurrent),
	/// unless m_pParent is nullptr, then ::GetParent(m_hCurrent) == nullptr
	node_t* m_pParent = nullptr;

	/// HWND to child nodes (node_t*)
	hwnd_to_children_ptr_t m_hwnd_to_child_node;

	/// Used to mark node for operations where don't want to duplicate
	bool m_bMarked = false;

	/// Used to count how many handles have been outputted.
	static size_t outputted_handle_count;

	node_t(HWND hCurrent, window_type_e type);
	verify_correctly_parented_e verify_correctly_parented() const;
	void add_child(node_t* pChild);
	void output_node_and_children(std::wostream& os, int indent = 0);

	static size_t m_nMax_depth;
	size_t calc_max_depth() const
	{
		size_t max_depth = 1;
		for (auto& child : m_hwnd_to_child_node) {
			max_depth = (std::max)(max_depth, child.second->calc_max_depth() + 1);
		}
		return max_depth;
	}
	size_t max_depth()
	{
		if (m_nMax_depth == -1) {
			auto this_ = this;
			while (this_->m_pParent) {
				this_ = this_->m_pParent;
			}
			m_nMax_depth = this_->calc_max_depth();
		}
		return m_nMax_depth;
	}
};

size_t node_t::outputted_handle_count = 0;
size_t node_t::m_nMax_depth = -1;

inline node_t::node_t(HWND hCurrent, window_type_e type)
	: m_eType(type)
	, m_hCurrent(hCurrent)
{}

inline verify_correctly_parented_e node_t::verify_correctly_parented() const
{
	if (m_hCurrent == nullptr && m_pParent == nullptr) {
		return correct;
	}

	HWND hParent = ::GetParent(m_hCurrent);
	auto found_parent_node = all_nodes.find(hParent);
	if (found_parent_node == all_nodes.end()) {
		return parent_node_not_found;
	}
	if (&found_parent_node->second == m_pParent) {
		return correct;
	}
	if (!hParent) {
		return parent_was_null;
	}
	return parent_was_different;
}

inline void node_t::add_child(node_t * pChild)
{
	bool inserted;
	decltype(m_hwnd_to_child_node)::iterator itChild;
	std::tie(itChild, inserted)
		= m_hwnd_to_child_node.try_emplace(pChild->m_hCurrent, pChild);
	// Should be newly inserted, or should be already point at this.
	assert(inserted && pChild->m_pParent == nullptr
		|| pChild->m_pParent == this);
	// These should point at the same node.
	assert(itChild->second == pChild);
	pChild->m_pParent = this;
}

// Output info for current and children nodes
inline void node_t::output_node_and_children(std::wostream & os, int indent)
{
	HWND hWnd = m_hCurrent;
	if (output_handle_count) {
		os << std::setw(7) << ++outputted_handle_count
			<< FIELD_SEPERATOR;
	}
	os << m_eType
		<< FIELD_SEPERATOR << verify_correctly_parented()
		<< FIELD_SEPERATOR << ::IsWindowVisible(hWnd)
		<< FIELD_SEPERATOR "\"";

	std::fill_n(std::ostreambuf_iterator<wchar_t>(os), indent, L' ');
	os << hWnd
		<< L"\"" FIELD_SEPERATOR;
	
	// padding
	//std::fill_n(std::ostreambuf_iterator<wchar_t>(os), max_depth()-1, L' ');
	//os << FIELD_SEPERATOR;

	output_window_class_and_title(os, hWnd);

	m_bMarked = true;

	// Output child node info
	for (auto& child_node : m_hwnd_to_child_node) {
		child_node.second->output_node_and_children(os, indent + 1);
	}
}

void allocate_node(HWND hwnd, window_type_e window_status)
{
	bool inserted;
	decltype(all_nodes)::iterator itNode;
	std::tie(itNode, inserted)
		= all_nodes.try_emplace(hwnd, hwnd, window_status);
	assert(inserted); // Shouldn't have to allocate twice.
}

// Check that parent HWND has a corrisponding node.  If not, allocate it and
// check it's parent recursively.
void ensure_all_nodes_to_root_allocated(HWND hWnd)
{
	auto found_node = all_nodes.find(hWnd);

	if (found_node == all_nodes.end()) {
		allocate_node(hWnd, not_enumable);
		found_node = all_nodes.find(hWnd);
		assert(found_node != all_nodes.end()); // Found node should now be valid
	}

	// Check if parent has been checked and if not, check it.
	if (!found_node->second.m_bMarked) {
		found_node->second.m_bMarked = true;
		if (HWND hParent = ::GetParent(hWnd)) {
			ensure_all_nodes_to_root_allocated(hParent);
		}
	}
}

// Some HWNDs may not have been iterated over, so go through all nodes and 
// ensure that their parent HWND has been allocated.
void ensure_all_nodes_to_root_allocated()
{
	for (auto& node : all_nodes) {
		ensure_all_nodes_to_root_allocated(node.first);
	}

	// Reset all the flagged nodes
	for (auto& node : all_nodes) {
		node.second.m_bMarked = false;
	}
}

// Add an HWND that the user wants specifically to look at.
void allocate_specific_node(HWND hwnd
	, window_type_e to_report = user_specified
	, bool ensure_all_parent_nodes_are_allocated = true
	, bool add_even_if_invalid = false)
{
	bool is_window = ::IsWindow(hwnd);
	if (is_window || add_even_if_invalid) {
		if (all_nodes.find(hwnd) == all_nodes.end()) {
			if (is_window) {
				allocate_node(hwnd, to_report);

				EnumChildWindows(hwnd,
					[](_In_ HWND hwnd, _In_ LPARAM to_report) -> BOOL
				{
					allocate_specific_node(hwnd, window_type_e(to_report), false);
					return TRUE;
				}, to_report);

				if (ensure_all_parent_nodes_are_allocated) {
					ensure_all_nodes_to_root_allocated(hwnd);
				}
			}
			else {
				allocate_node(hwnd, invalid);
			}
		}
	}
}

// Allocate message only nodes.
void allocate_message_only_nodes()
{
	HWND last_child = NULL;
	while (HWND found = FindWindowEx(HWND_MESSAGE, last_child, NULL, NULL)) {
		allocate_node(found, message);
		last_child = found;
	}
}
// This allows compiling on 32 or 64 bit system.
using hwnd_numeric_t = std::conditional_t<sizeof(HWND) == sizeof(int32_t), int32_t, int64_t>;

// Allocate nodes by finding them using a brute force method.
// I think most of the handles recovered are fictitious.
void allocate_brute_force()
{
	// Strange.  constexpr doesn't work here.  BUG?
	static const auto max_hwnd 
		= (HWND)((std::numeric_limits<hwnd_numeric_t>::max)() >>
			(sizeof(HWND) == sizeof(int32_t)
				? 0
				: 32-4 // this would be too big and take too long if I did all of them
			));
	for (HWND hwnd = 0; hwnd < max_hwnd; (hwnd_numeric_t&)hwnd += 2) {
		if ((hwnd_numeric_t(hwnd) & 0xfffff) == 0) {
			auto percent = double((hwnd_numeric_t)hwnd) / (hwnd_numeric_t)max_hwnd * 100.0;
			std::cerr.precision(2);
			std::cerr << std::setw(6) << std::fixed << std::setprecision(2) << percent << "%\r" << std::flush;
		}

		allocate_specific_node(hwnd, brute_force, false);
	}
	std::cerr << "       \r" << std::flush;
}

// Make a node for each HWND currently in the system.
void allocate_nodes(bool brute_force)
{
	allocate_message_only_nodes();
	allocate_node(nullptr, special);
	allocate_node(GetDesktopWindow(), special);

	EnumWindows([](_In_ HWND hwnd, _In_ LPARAM lParam) -> BOOL
	{
		assert(hwnd);
		allocate_node(hwnd, top_level);
		EnumChildWindows(hwnd, [](_In_ HWND hwnd, _In_ LPARAM lParam) -> BOOL
		{
			allocate_node(hwnd, not_top_level);
			return TRUE;
		}, 0);
		return TRUE;
	}
	, 0);

	if (brute_force) {
		allocate_brute_force();
	}

	ensure_all_nodes_to_root_allocated();
}

// Get the associated node for an HWND.
node_t* get_node(HWND hWnd) {
	auto found_parent = all_nodes.find(hWnd);
	assert(found_parent != all_nodes.end());
	return &found_parent->second;
}

// Attaches all nodes to their parent nodes.
void attach_parent_nodes_to_their_children()
{
	// Build node graph
	for (auto& node : all_nodes) {
		// Keep NULL HWND making node reference itself
		if (HWND hWnd = node.first) {
			auto pWnd = get_node(hWnd);
			assert(pWnd);
			assert(pWnd->m_hCurrent);
			auto pParent = get_node(::GetParent(hWnd));
			pParent->add_child(pWnd);
		}
	}
}

// Output all of the node information to the output stream.
void output_node_structure(std::wostream& os)
{
	all_nodes.find(nullptr)->second.output_node_and_children(os);
}

// Outputs broken relationships if any. Shouldn't result in any output.
void output_broken_node_structure(std::wostream& os)
{
	bool found_broken;
	do {
		found_broken = false;
		for (auto& node : all_nodes)
		{
			node_t* root = &node.second;
			assert(root);
			if (!root->m_bMarked)
			{
				found_broken = true;

				while (root->m_hCurrent && root->m_pParent) {
					root = root->m_pParent;
				}

				os << "Broken child/parent relationship!\n";
				root->output_node_and_children(os);
				os << std::endl;
			}
		}
	} while (found_broken);
}

// Setting the locale on the stream prevents crapping out when passed an mdash.
// wchar_t mdash[] = { 0x2014, 0x0000 };
// Side effect is that numbers now have grouping separators, even in hex! >:(
/*
void set_locale_on_stream(std::wfstream &os)
{
	char* locale = setlocale(LC_ALL, "English"); // Get the CRT's current locale.
	std::locale lollocale(locale);
	setlocale(LC_ALL, nullptr); // Restore the CRT.
	os.imbue(lollocale); // Now set the std::wcout to have the locale that we got from the CRT.
}
*/

//#if defined(_DEBUG)
//	wchar_t mdash[] = { 0x2014, 0x0000 };
//	os << CW2A(mdash);
//	assert(!os.bad());
//#endif

int main()
{
	std::wfstream os("window-tree.txt" , std::ios_base::out | std::ios_base::trunc);
	os.exceptions(os.badbit | os.failbit | os.eofbit);

	output_handle_count = true;
	allocate_nodes(false);
	attach_parent_nodes_to_their_children();
	try {
		output_heading(os);
		output_node_structure(os);
		output_broken_node_structure(os);
	}
	catch (std::ios_base::failure& e) {
		std::cerr << "Exception: " << e.what() << std::endl;
	}
	return 0;
}
