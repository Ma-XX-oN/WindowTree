#include "stdafx.h"

// WindowTree.cpp : Defines the entry point for the console application.
//
#include <AtlBase.h> // Conversion routines (CW2A)
#include <Windows.h> // Windows stuff
//#include <WinUser.h>
//#include <unordered_map>
#include <assert.h>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <regex>
//#include <type_traits>
#include <set>
#include <psapi.h>
#include "WindowTree.h"

// Characters to use as a filed seperator.
// Used define so can use c-string concatonation.
#define FIELD_SEPERATOR L","
#if 1
bool output_handle_count = false;
POINT pt = {};

struct node_t;

using hwnd_to_allocated_childern_t = my_map<HWND, node_t>;

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

std::wostream& operator<<(std::wostream& os, verify_correctly_parented_e value)
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
		os << "handle  " FIELD_SEPERATOR;
	}
	//handle ! window_status!expected!v!tree               !O!o!owner   !parentGP!parent  !parentGA! pid! tid!process name!class name!window title
	os << " window_status"
		FIELD_SEPERATOR "expected"
		FIELD_SEPERATOR "v" // visible
		FIELD_SEPERATOR "tree               "
		FIELD_SEPERATOR "T" // Is top level  (m_hCurrent == GetAncestor(m_hCurrent, GA_ROOT))
		FIELD_SEPERATOR "t" // Is top level !(GetWindowLong(m_hCurrent, GWL_STYLE) & WS_CHILD)
		FIELD_SEPERATOR "owner"
		FIELD_SEPERATOR "parentGP" // GetParent()
		FIELD_SEPERATOR "parent"
		FIELD_SEPERATOR "parentGA" // GetAncestor()
		FIELD_SEPERATOR " pid"
		FIELD_SEPERATOR " tid"
		FIELD_SEPERATOR "process name"
		FIELD_SEPERATOR "class name"
		FIELD_SEPERATOR "window title"
		"\n";
}

#endif

auto& output_window_class_and_title(std::wostream & os, const HWND &hWnd)
{
	wchar_t window_class[1024], window_title[1024];
	window_class[0] = window_title[0] = 0;

	::GetClassNameW(hWnd, window_class, _countof(window_class));
	::GetWindowTextW(hWnd, window_title, _countof(window_title));
	// replacing any CRLFs with field separators
	auto wc
		= std::regex_replace(window_class, std::wregex(L"(\r\n?|\n\r?)")
			, L" " );
	auto wt
		= std::regex_replace(window_title, std::wregex(L"(\r\n?|\n\r?)")
			, L" " );

	os << CW2A(wc.c_str()) << FIELD_SEPERATOR << CW2A(wt.c_str());
	return os;
}

// Store exe names
std::set<std::wstring> exe_names;

// Map pid to exe name
std::map<DWORD, std::wstring const*> pid_to_exe_name;

// Get exe name
const std::wstring * GetProcessName(DWORD pid)
{
	const std::wstring * pProcess_name = nullptr;
	auto it_found_pid = pid_to_exe_name.lower_bound(pid);
	if (it_found_pid == pid_to_exe_name.end() || it_found_pid->first != pid) {
		wchar_t exe_name[MAX_PATH]; exe_name[0] = 0;
		if (HANDLE hProcess = ::OpenProcess(
			PROCESS_ALL_ACCESS | PROCESS_QUERY_INFORMATION |
			PROCESS_VM_READ,
			FALSE, pid))
		{
			auto chars_copied = ::GetProcessImageFileNameW(hProcess, exe_name, _countof(exe_name));
			assert(chars_copied > 0);
			exe_name[chars_copied] = 0;
			::CloseHandle(hProcess);
			auto found = exe_names.emplace(exe_name);
			pProcess_name = &*found.first;
		}
		else
		{
			auto found = exe_names.emplace(L"* Couldn't open process handle *");
			pProcess_name = &*found.first;
		}
		pid_to_exe_name.emplace_hint(it_found_pid, pid, pProcess_name);
	}
	else {
		pProcess_name = it_found_pid->second;
	}
	return pProcess_name;
}

std::set<std::wstring> class_names;

const std::wstring * get_window_class_name(HWND hwnd)
{
	wchar_t className[256]; className[0] = 0;
	::GetClassNameW(hwnd, className, 256);
	auto itFound = class_names.lower_bound(className);
	std::wstring const* result;
	if (itFound != class_names.end() && *itFound == className) {
		result = &*itFound;
	}
	else {
		result = &*class_names.emplace_hint(itFound, className);
	}
	return result;
}

std::wstring get_window_text(HWND hwnd)
{
	auto len = GetWindowTextLengthW(hwnd);
	std::wstring text;
	text.resize(len);
	GetWindowTextW(hwnd, &text[0], len);
	return text;
}

#if 1
// Node allocation done here.  They are referenced by HWND.
hwnd_to_allocated_childern_t all_nodes;


size_t node_t::outputted_handle_count = 0;
size_t node_t::m_nMax_depth = -1;

inline node_t::node_t(HWND hCurrent, window_type_e type)
	: m_eType(type)
	, m_hCurrent(hCurrent)
{
	set_other_attributes();
}

node_t::node_t(HWND hCurrent, node_t* pChild)
	//: m_hCurrent(hCurrent)
{
	bool inserted;
	hwnd_to_children_ptr_t::iterator itChild;
	std::tie(itChild, inserted)
		= m_hwnd_to_child_node.try_emplace(pChild->m_hCurrent, pChild);
	assert(inserted); // Should never insert same node multiple times.
	pChild->m_pParent = this;
}

void node_t::add_info(HWND hCurrent, window_type_e type)
{
	assert(!m_hCurrent); // Don't add info to a node where the info is already added.
	m_eType = type;
	m_hCurrent = hCurrent;
	set_other_attributes();
}

void node_t::set_other_attributes()
{
	return;
	if (m_hCurrent) {
		DWORD pid;
		auto tid = ::GetWindowThreadProcessId(m_hCurrent, &pid);
		m_pid = pid;
		m_tid = tid;
		m_pExe_name = GetProcessName(pid);
	}
	else {
		m_pid = m_tid = 0;
		auto found = exe_names.emplace(L"* Root handle has no associated process *");
		m_pExe_name = &*found.first;
	}
	m_pClass_name = get_window_class_name(m_hCurrent);
	m_title = get_window_text(m_hCurrent);
	m_bVisible = ::IsWindowVisible(m_hCurrent);
}

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
	//assert(inserted && pChild->m_pParent == nullptr
	//	|| pChild->m_pParent == this);
	assert(inserted); // Should only be newly inserted
	// These should point at the same node.
	assert(itChild->second == pChild);
	pChild->m_pParent = this;
}

// Output info for current and children nodes
inline void node_t::output_node_and_children(std::wostream & os, int indent)
{
	if (!m_bMarked) {
		HWND hWnd = m_hCurrent;
		if (output_handle_count) {
			os << std::setw(7) << ++outputted_handle_count
				<< FIELD_SEPERATOR;
		}
		RECT rect;
		auto get_rect_success = ::GetWindowRect(m_hCurrent, &rect);
		os << m_eType
			<< FIELD_SEPERATOR << verify_correctly_parented()
			<< FIELD_SEPERATOR << ::IsWindowVisible(hWnd)
			<< FIELD_SEPERATOR << value;
		size_t min_coord_width = 7; // 10;
		if (get_rect_success && rect.right - rect.left > 0 && rect.bottom - rect.top > 0) {
			os	<< FIELD_SEPERATOR << "("
				<< std::setw(min_coord_width) << rect.left << ", "
				<< std::setw(min_coord_width) << rect.top << ", "
				<< std::setw(min_coord_width) << rect.right << ", "
				<< std::setw(min_coord_width) << rect.bottom << ")"
				<< FIELD_SEPERATOR << "("
				<< std::fixed << std::setw(9) << std::setprecision(2) << (((pt.x - rect.left) * 100.0) / (rect.right - rect.left)) << "%, "
				<< std::fixed << std::setw(9) << std::setprecision(2) << (((pt.y - rect.top ) * 100.0) / (rect.bottom - rect.top)) << "%)";
		}
		else {
			os << FIELD_SEPERATOR; //<< "(";
			std::fill_n(std::ostream_iterator<wchar_t const*, wchar_t>(os), 4, L"         ");
			os << FIELD_SEPERATOR; //<< "(";
			std::fill_n(std::ostream_iterator<wchar_t const*, wchar_t>(os), 2, L"            ");
		}
		os
			<< FIELD_SEPERATOR "\"";

		std::fill_n(std::ostreambuf_iterator<wchar_t>(os), indent, L' ');
		os << hWnd
			<< L"\"";

		// padding
		std::fill_n(std::ostreambuf_iterator<wchar_t>(os), max_depth_from_root() - 1 - indent, L' ');

		auto hOwner                   = (HWND)::GetWindow(m_hCurrent, GW_OWNER);
		auto hParent                  = (HWND)::GetWindowLongPtr(m_hCurrent, GWLP_HWNDPARENT);
		auto hParent_from_GetParent   = ::GetParent(m_hCurrent);
		auto hParent_from_GetAncestor = ::GetAncestor(m_hCurrent, GA_PARENT);
		bool is_top_level  = (m_hCurrent == GetAncestor(m_hCurrent, GA_ROOT));
		bool is_top_level2 = !(GetWindowLong(m_hCurrent, GWL_STYLE) & WS_CHILD);

		os << std::hex
			//<< FIELD_SEPERATOR << std::setw(8) << (m_pParent ? m_pParent->m_hCurrent : nullptr)
			<< FIELD_SEPERATOR << is_top_level
			<< FIELD_SEPERATOR << is_top_level2
			<< FIELD_SEPERATOR << std::setw(8) << hOwner
			<< FIELD_SEPERATOR << std::setw(8) << hParent_from_GetParent
			<< FIELD_SEPERATOR << std::setw(8) << hParent
			<< FIELD_SEPERATOR << std::setw(8) << hParent_from_GetAncestor
			<< FIELD_SEPERATOR << std::setw(4) << m_pid
			<< FIELD_SEPERATOR << std::setw(4) << m_tid
			<< FIELD_SEPERATOR << (m_pExe_name ? *m_pExe_name : L"nullptr") << std::dec
			<< FIELD_SEPERATOR;

		output_window_class_and_title(os, hWnd) << std::endl;

		m_bMarked = true;

		// Output child node info
		for (auto& child_node : m_hwnd_to_child_node) {
			child_node.second->output_node_and_children(os, indent + 1);
		}
	}
}

inline size_t node_t::max_depth_from_here() const
{
	size_t max_depth = 1;
	for (auto& child : m_hwnd_to_child_node) {
		max_depth = (std::max)(max_depth, child.second->max_depth_from_here() + 1);
	}
	return max_depth;
}

inline node_t * node_t::get_root()
{
	auto this_ = this;
	while (this_->m_pParent) {
		this_ = this_->m_pParent;
	}
	return this_;
}

inline size_t node_t::max_depth_from_root()
{
	if (m_nMax_depth == -1) {
		auto this_ = get_root();
		m_nMax_depth = this_->max_depth_from_here();
	}
	return m_nMax_depth;
}

void allocate_node(HWND hwnd, window_type_e window_status)
{
	bool inserted;
	decltype(all_nodes)::iterator itNode;
	std::tie(itNode, inserted)
		= all_nodes.try_emplace(hwnd, hwnd, window_status);
	if (!inserted) {
		assert(itNode->second.m_hCurrent == nullptr);
		itNode->second.add_info(hwnd, window_status);
	}

	// If root HWND, don't add parent
	if (hwnd) {
		decltype(all_nodes)::iterator itParentNode;
		auto hParent = ::GetParent(hwnd);
		std::tie(itParentNode, inserted)
			= all_nodes.try_emplace(hParent, hParent, &itNode->second);
		if (!inserted) {
			itParentNode->second.add_child(&itNode->second);
		}
	}
}

// Check that parent HWND has a corrisponding node.  If not, allocate it and
// check it's parent recursively.
void ensure_all_nodes_to_root_allocated(HWND hWnd)
{
	auto found_node = all_nodes.find(hWnd);

	assert(found_node != all_nodes.end());
	if (found_node->second.m_hCurrent == nullptr) {
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

void clear_mark()
{
	// Reset all the flagged nodes
	for (auto& node : all_nodes) {
		node.second.m_bMarked = false;
		node.second.value = 0;
	}
}

void set_mark()
{
	// Reset all the flagged nodes
	for (auto& node : all_nodes) {
		node.second.m_bMarked = true;
		node.second.value = 0;
	}
}

// Some HWNDs may not have been iterated over, so go through all nodes and 
// ensure that their parent HWND has been allocated.
void ensure_all_nodes_to_root_allocated()
{
	for (auto& node : all_nodes) {
		ensure_all_nodes_to_root_allocated(node.first);
	}

	clear_mark();
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
	allocate_node(nullptr, special);
	allocate_node(GetDesktopWindow(), special);
	allocate_message_only_nodes();

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

void build_tree()
{
	all_nodes.clear();
	allocate_nodes(false);
//	attach_parent_nodes_to_their_children();
}

void output_window_tree(std::wostream &os)
{
	output_handle_count = true;
	try {
		output_heading(os);
		output_node_structure(os);
		output_broken_node_structure(os);
	}
	catch (std::ios_base::failure& e) {
		std::cerr << "Exception: " << e.what() << std::endl;
	}
}

void output_window_tree(const char * filename)
{
	std::wfstream os(filename, std::ios_base::out | std::ios_base::trunc);
	os.exceptions(os.badbit | os.failbit | os.eofbit);

	output_window_tree(os);
}

#endif

int main()
{
	build_tree();

	char response;
	do {
		POINT ptLast;
		std::cout << "Find all windows and parents at coord:" << std::endl;
		int count = 0;
		do {
			ptLast = pt;
			::GetCursorPos(&pt);
			if (ptLast.x == pt.x && ptLast.y == pt.y) {
				++count;
			}
			else {
				count = 0;
			}
			std::cout
				<< "("
				<< std::setw(10) << pt.x << ", "
				<< std::setw(10) << pt.y << ") "
				<< std::setw(8) << count << "\r";
			std::flush(std::cout);
			::Sleep(1000);
		} while (count < 5);

		{
			auto* filename = "window-tree.txt";
			static std::wfstream os(filename, std::ios_base::out | std::ios_base::trunc);
			//static auto& os = std::wcout;
			os.exceptions(os.badbit | os.failbit | os.eofbit);
			clear_mark();

			build_tree();
			output_window_tree(filename);

			std::cout << std::endl;
			set_mark();
			for (auto& hwnd_node_pair : all_nodes) {
				RECT rect;
				auto& node = hwnd_node_pair.second;
				auto hwnd = node.m_hCurrent;
				if (::IsWindowVisible(hwnd) && ::GetWindowRect(hwnd, &rect)) {
					if (::PtInRect(&rect, pt)) {
						node.value = 1;
						node.m_bMarked = false;
						auto* parent_node = node.m_pParent;
						while (parent_node && parent_node->m_bMarked) {
							parent_node->m_bMarked = false;
						}
					}
				}
			}
			output_window_tree(std::wcout);
		}
		std::cout << "Again (y/n)?" << std::endl;
		do {
			std::cin >> response;
		} while (response != 'y' && response != 'n');
	} while (response == 'y');
	return 0;
}
